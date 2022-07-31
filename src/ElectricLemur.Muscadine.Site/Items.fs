module ElectricLemur.Muscadine.Site.Items

open Microsoft.AspNetCore.Http
open System.Threading.Tasks
open Giraffe.ViewEngine
open Microsoft.FSharp.Reflection
open Newtonsoft.Json.Linq

type _ImagePath(pathIn: string) =
    let path = pathIn
    member this.Path = path

type _FieldType =
    /// The field is represented in the UI by a text box and backed by a string or string option in the model
    | TextField_
    /// The field is represented in the UI by a check box and backed by a bool or bool option in the model
    | CheckboxField_
    /// The field is represented in the UI by a date field and backed by a DateTimeOffset or DateTimeOffset option in the model
    | DateTime_
    /// The field is represented in the UI by a drag-and-drop image field and backed by an ImagePath or ImagePath option in the model. The file path points to a location on disk.
    | Image_

type _RequiredField =
    /// The field is not wrapped by an option in the model
    | Required_
    /// The field is represented in the model as an Option<'a>
    | NotRequired_

type _Field = {
    Name: string;
    FieldType: _FieldType;
    Required: _RequiredField;
}

let private _getModelFieldType (f: System.Reflection.PropertyInfo) =
    let stringType = "".GetType()
    let stringOptionType = (Some "").GetType()
    let boolType = true.GetType()
    let boolOptionType = (Some true).GetType()
    let dateTimeType = System.DateTimeOffset.UtcNow.GetType()
    let dateTimeOptionType = (Some System.DateTimeOffset.UtcNow).GetType()
    let imagePathType = (new _ImagePath("")).GetType()
    let imagePathOptionType = (Some (new _ImagePath(""))).GetType()

    match f.PropertyType with
    | t when t = stringType -> (TextField_, Required_)
    | t when t = stringOptionType -> (TextField_, NotRequired_)
    | t when t = boolType -> (CheckboxField_, Required_)
    | t when t = boolOptionType -> (CheckboxField_, NotRequired_)
    | t when t = dateTimeType -> (DateTime_, Required_)
    | t when t = dateTimeOptionType -> (DateTime_, NotRequired_)
    | t when t = imagePathType -> (Image_, Required_)
    | t when t = imagePathOptionType -> (Image_, NotRequired_)
    | _ -> failwith $"Unknown record field type type %s{f.PropertyType.FullName}"

let private _modelFields<'a> = 
    let fields = 
        let fail msg = failwith $"Type %s{(typedefof<'a>).FullName} %s{msg}"
        try
            let fields = FSharpType.GetRecordFields(typedefof<'a>)
            if fields.Length <= 2 then fail "does not contain enough properties"

            let idField = fields.[0]
            if idField.Name <> "Id" then fail "does not contain an Id property"
            if idField.PropertyType.Name <> "String" then fail "does not contain an Id: string property"

            let dateField = fields.[1]
            if dateField.Name <> "DateAdded" then fail "does not contain a DateAdded property"
            if dateField.PropertyType.Name <> "DateTimeOffset" then fail "does not contain a DateAdded: DateTimeOffset property"

            fields |> Array.skip 2
        with
        | :? System.ArgumentException -> fail "is not a record type"

    fields
    |> Array.toSeq
    |> Seq.map (fun f ->
        let (t, req) = _getModelFieldType f
        {
            Name = f.Name
            FieldType = t
            Required = req
        }
    )
    
let _databaseFields = [ 
    { Name = "_id"; FieldType = TextField_; Required = Required_}; 
    { Name = "_dateAdded"; FieldType = DateTime_; Required = Required_};
]


let private _getFieldValueFromJObject (obj: JObject) fieldType : obj =
    match (fieldType.FieldType, fieldType.Required, fieldType.Name) with
    | (TextField_, Required_, n) -> n |> JObj.getter<string> obj |> Option.get |> box
    | (TextField_, NotRequired_, n) -> n |> JObj.getter<string> obj |> box

    | (CheckboxField_, Required_, n) -> n |> JObj.getter<bool> obj |> Option.get |> box
    | (CheckboxField_, NotRequired_, n) -> n |> JObj.getter<bool> obj |> box

    | (DateTime_, Required_, n) -> n |> JObj.getter<System.DateTimeOffset> obj |> Option.get |> box
    | (DateTime_, NotRequired_, n) -> n |> JObj.getter<System.DateTimeOffset> obj |> box

    | (Image_, Required_, n) -> n |> JObj.getter<string> obj |> Option.get |> _ImagePath |> box
    | (Image_, NotRequired_, n) -> n |> JObj.getter<string> obj |> Option.map _ImagePath |> box
            

/// Converts a JObject, obj, to a model object
/// Throws an exception if obj does not contain all required fields for the model
/// Throws an exception if the type 'a does not conform to expectations provided by the Field type
let _makeModelFromJObject<'a> (obj: JObject) =
    let fieldTypes = _modelFields<'a>
    let requiredFields = fieldTypes |> Seq.filter (fun f -> f.Required = Required_) |> Seq.append _databaseFields // JObjects come from the database so need the DB fields
    let fieldExists f = JObj.fieldExists obj f.Name
    let missing = requiredFields |> Seq.filter (fieldExists >> not)

    if missing |> Seq.isEmpty then
        let constructorValues =
            fieldTypes
            |> Seq.append _databaseFields
            |> Seq.map (_getFieldValueFromJObject obj)
            |> Seq.toArray
        let m = FSharpValue.MakeRecord(typedefof<'a>, constructorValues)
        m :?> 'a
    else
        let missing = System.String.Join(",", (missing |> Seq.map (fun f -> f.Name)))
        failwith $"JObject missing fields [%s{missing}] for model type %s{(typedefof<'a>).FullName}"

/// Converts a model object, m, to a JObject. 
/// Throws an exception if the model object does not conform to expectations provided by the Field type
let _makeJObjectFromModel (m: 'a) (documentType: string) =
    let fieldTypes = _modelFields<'a> |> Seq.append _databaseFields // JObjects are saved to the database so need the DB fields
    let values = FSharpValue.GetRecordFields(m)

    let r = 
        Seq.zip fieldTypes values 
        |> Seq.fold (fun (acc: JObject) (fieldType, v) -> 
            match (fieldType.FieldType, fieldType.Required, fieldType.Name) with
                | (TextField_, Required_, n) -> acc.[n] <- (v :?> string)
                | (TextField_, NotRequired_, n) -> 
                    let v = v :?> string option
                    match v with
                    | Some v -> acc.[n] <- v
                    | _ -> ()
                | (CheckboxField_, Required_, n) -> acc.[n] <- (v :?> bool)
                | (CheckboxField_, NotRequired_, n) -> 
                    let v = v :?> bool option
                    match v with
                    | Some v -> acc.[n] <- v
                    | _ -> ()

                | (DateTime_, Required_, n) -> acc.[n] <- (v :?> System.DateTimeOffset).ToString("o")
                | (DateTime_, NotRequired_, n) ->
                    let v = v :?> System.DateTimeOffset option
                    match v with
                    | Some v -> acc.[n] <- v.ToString("o")
                    | _ -> ()

                | (Image_, Required_, n) -> acc.[n] <- (v :?> _ImagePath).Path
                | (Image_, NotRequired_, n) ->
                    let v = v :?> _ImagePath option
                    match v with
                    | Some v -> acc.[n] <- v.Path
                    | _ -> ()
            acc) 
            (new JObject())

    r.["_documentType"] <- documentType
    r

/// Attempts to convert a form data set to a model object. 
/// Throws an exception if the type 'a does not conform to expectations provided by the Field type
/// Returns an Error result if data is missing or cannot be converted from the form data set
let _makeModelFromFormFields<'a> (id: string) (dateAdded: System.DateTimeOffset) (form: IFormCollection) : Result<'a, string> =
    let fieldTypes = _modelFields<'a>
    let requiredFields = fieldTypes |> Seq.filter (fun f -> f.Required = Required_)
    let fieldExists f = form.ContainsKey(f.Name) || f.FieldType = CheckboxField_ // booleans are weird because the browser just doesn't send them up when they are false
    let missing = requiredFields |> Seq.filter (fieldExists >> not) 

    let extractFieldValue fieldType : obj option =
        if form.ContainsKey(fieldType.Name) then
            let s = box(form.[fieldType.Name])
            let s = if s = null then None else Some (string s)

            match (fieldType.FieldType, fieldType.Required) with
            | TextField_, Required_ ->
                match s with
                | Some s -> Some (box s)
                | None -> None
            | TextField_, NotRequired_ ->
                match s with
                | Some s -> Some (Some (box s))
                | None -> Some (None)
            | CheckboxField_, Required_ ->
                match s with
                | Some s when s = "on" || s = "true" -> Some true
                | Some s when s = "false" -> Some false
                | Some _ -> Some true
                | _ -> Some false
            | CheckboxField_, NotRequired_ ->
                match s with
                | Some s when s = "on" || s = "true" -> Some (Some true)
                | Some s when s = "false" -> Some (Some false)
                | Some _ -> Some (Some true)
                | _ -> Some (Some false)
            | DateTime_, Required_ ->
                match s with
                | Some s ->
                    match System.DateTimeOffset.TryParse(s) with
                    | true, d -> Some (box s)
                    | false, _ -> None
                | None -> None
            | DateTime_, NotRequired_ ->
                match s with
                | Some s ->
                    match System.DateTimeOffset.TryParse(s) with
                    | true, d -> Some (Some (box d))
                    | false, _ -> Some (None)
                | None -> Some (None)
            | Image_, Required_ -> raise (new System.NotImplementedException()) //TODO
            | Image_, NotRequired_ -> raise (new System.NotImplementedException()) //TODO
        else
            // booleans are weird since the browser doesn't send them up if they're false
            match fieldType.FieldType, fieldType.Required with
            | CheckboxField_, Required_ -> Some(false)
            | CheckboxField_, NotRequired_ -> Some (Some false)
            | _ -> None

    if (missing |> Seq.isEmpty |> not) then
        let missing = System.String.Join(", ", (missing |> Seq.map (fun f -> f.Name)))
        Error $"Form data is missing required fields: [%s{missing}]"
    else
        let values = fieldTypes |> Seq.map (fun f -> (f, extractFieldValue f))
        let nones = values |> Seq.filter (fun (_, v) -> v |> Option.isNone) |> Seq.map fst
        if nones |> Seq.isEmpty then
            let constructorValues = values |> Seq.map snd |> Seq.map Option.get |> Seq.toArray
                
            // need to add the database values in since the constructor will expect them 
            let constructorValues = Array.append [| box id; box dateAdded; |] constructorValues
            let t = typedefof<'a>
            let m = FSharpValue.MakeRecord(typedefof<'a>, constructorValues)
            let m = m :?> 'a
            Ok m
        else
            let nones = System.String.Join(", ", (nones |> Seq.map (fun f -> f.Name)))
            Error $"Unable to convert form data to correct type for [%s{nones}]"

let _modelFieldType<'a> fieldName = 
    let fields = _modelFields<'a>
    let field = fields |> Seq.find (fun f -> f.Name = fieldName)
    (field.FieldType, field.Required)

let private _modelObjValue (m: 'a) (fieldName: string) =
    let fieldInfos = FSharpType.GetRecordFields(typedefof<'a>)
    let fieldInfo = fieldInfos |> Array.find (fun i -> i.Name = fieldName)
    FSharpValue.GetRecordField(m, fieldInfo)

let private _modelValue (m: 'a) (fieldName: string) (converter: obj -> 'b) =
    let value = _modelObjValue m fieldName
    let (_, isRequired) = _modelFieldType<'a> fieldName

    match value with
    | null -> None
    | _ -> 
        match isRequired with
        | Required_ -> Some (converter value)
        | NotRequired_ -> 
            let value = value :?> 'b option
            match value with
            | Some value -> 
                match box value with
                | null -> None
                | _ -> Some (converter value)
            | None -> None

let _modelStringValue (m: 'a) (fieldName: string) = _modelValue m fieldName string
let _modelBoolValue (m: 'a) (fieldName: string) = _modelValue m fieldName System.Convert.ToBoolean
let _modelImagePathValue (m: 'a) (fieldName: string) = _modelValue m fieldName (fun o -> o :?> _ImagePath)

let _validateRequiredFields (prev: Result<'a, string>) =
    let stringFieldIsEmpty (v: obj) =
        (box v = null) || (System.String.IsNullOrWhiteSpace(string v))

    let boolFieldIsEmpty (v: obj) =
        (box v = null)

    let dateFieldIsEmpty (v: obj) =
        if box v = null then true
        else
            match System.DateTimeOffset.TryParse(string v) with
            | true, _ -> false
            | false, _ -> true

    let imagePathFieldIsEmpty (v: obj) =
        (box v = null) || (System.String.IsNullOrWhiteSpace((v :?> _ImagePath).Path))

    let validateRequiredField (prev: Result<'a, string>) f isEmpty =
        match prev with
        | Error s -> Error s
        | Ok m -> 
            let fieldValue = _modelObjValue m f.Name
            if isEmpty fieldValue then 
                Error $"%s{f.Name} cannot be empty"
            else
                Ok m

    match prev with
    | Error s -> Error s
    | Ok m -> 
        let fieldTypes = _modelFields<'a>
        let requiredFields = fieldTypes |> Seq.filter (fun f -> f.Required = Required_)
        requiredFields 
        |> Seq.fold (fun prev f ->
            match f.FieldType with
            | TextField_ -> validateRequiredField prev f stringFieldIsEmpty
            | CheckboxField_ -> validateRequiredField prev f boolFieldIsEmpty
            | DateTime_-> validateRequiredField prev f dateFieldIsEmpty
            | Image_ -> validateRequiredField prev f imagePathFieldIsEmpty
        ) (Ok m)



let performValidation (f: 'a -> Result<'a, string>) (prev: Result<'a, string>) =
    match prev with
    | Error s -> Error s
    | Ok g -> f g

let performValidationAsync (f: 'a -> Task<Result<'a, string>>) (prev: Result<'a, string>) = 
    match prev with
    | Error s -> Task.FromResult(Error s)
    | Ok g -> task {
        return! f g
    }

type pageDataType =
    | String of value: string
    | Int of value: int
    | Float of value: float

let layout pageTitle pageData content =
    let makePageDataScript =
        let sb = new System.Text.StringBuilder()
        let sb = sb.AppendLine("window.pageData = {};")

        let sb =
            pageData
            |> Map.fold (fun (sb: System.Text.StringBuilder) k v ->

                let sb = sb.Append($"window.pageData[\"%s{k}\"] = ")
                let sb = sb.Append(match v with
                                   | String s -> $"\"%s{s}\""
                                   | Int i -> $"%d{i}"
                                   | Float f -> $"%f{f}")
                sb.AppendLine(";")) sb
        sb.ToString()

    html [] [
        head [] [
            title [] [ encodedText pageTitle ]
            link [ (_rel "stylesheet"); (_type "text/css"); (_href "/css/admin.scss") ]
            script [] [ rawText makePageDataScript ]
            script [ _src "/js/admin.js" ] []
        ]
        body [] [
            div [ _class "site-title" ] [
                encodedText "James Williams/"
                a [ _href "/admin/" ] [encodedText "Admin"]
            ]
            div [ _class "body-content" ] content
        ]
    ]

let makeInputRow label formEl = 
    tr [] [
        td [ _class "form-label" ] [ encodedText label ]
        td [ _class "form-input" ] [ formEl ]
    ]

let makeImageInputRow label key pathToDisplay =
    let src = 
        match Util.addRootPath "/images" pathToDisplay with 
        | Some p -> p
        | None -> ""

    let el = div 
                [ _class "image-input"
                  attr "data-image-key" key ]
                [ 
                    input [ 
                        _type "file" 
                        _style "display: none"
                        _name key
                        _id key
                    ]
                    img [
                        _src src
                    ] 
                ]
    makeInputRow label el

let makeTextAreaInputRow label key value =
    let el =
        textarea [
            _id key
            _name key
            _rows "5"
            _cols "30"
        ] [ encodedText (value |> Option.defaultValue "")]

    makeInputRow label el

let makeTextInputRow label key value =
    let el = input [
        _type "text"
        _id key
        _name key
        _value (value |> Option.defaultValue "")
    ]
    makeInputRow label el

let makeCheckboxInputRow label key (value: bool option) = 
    let value = value |> Option.defaultValue false
    let attributes = 
        [
            _type "checkbox"
            _id key
            _name key
            _value "on"
        ]
        |> Util.appendToListIf value _checked

    let el = input attributes
    makeInputRow label el

let makeTagsInputRow label key (allTags: string seq) (documentTags: string seq) =
    let selectAttributes = [
        _name key
        _id key
        _multiple
        _size "10"
    ]

    let allTags = Seq.append allTags documentTags |> Seq.distinct |> Seq.sort |> Seq.toList
    let tagIsSelected t = documentTags |> Seq.contains t

    let el = 
        div [ _class "tags-container" ] [
            select selectAttributes (
                allTags 
                |> List.map (fun t -> 
                    let attributes = 
                        [ _value t ]
                        |> Util.appendToListIf (tagIsSelected t) _selected

                    option attributes [ encodedText t ]))

            div [ _class "new-tag-container"] [
                input [ _type "text"; _class "new-tag-field"; _id "new-tag-field"; _name "new-tag-field" ]
                button [ _class "new-tag-button" ] [ encodedText "Add Tag"]
            ]
        ]
    makeInputRow label el

let _makeInputRowForFormElement (model: 'a option) (label: string) key =
    let (fieldType, req) = _modelFieldType<'a> key
    let label = if req = Required_ && fieldType <> CheckboxField_ then
                    if label.EndsWith("*") then label else $"%s{label}*"
                else
                    label

    let stringValue key = model |> Option.map (fun m -> _modelStringValue m key) |> Option.flatten
    let boolValue key = model |> Option.map (fun m -> _modelBoolValue m key) |> Option.flatten

    match fieldType with
    | TextField_ -> makeTextInputRow label key (stringValue key)
    | DateTime_ -> failwith "Unsupported field type, DateTime" //TODO
    | CheckboxField_ -> makeCheckboxInputRow label key (boolValue key)
    | Image_ -> raise (new System.NotImplementedException()) // TODO

let requiredStringValidator fieldName getter m =
    if (System.String.IsNullOrWhiteSpace(getter m)) then
        Error $"%s{fieldName} is required and cannot be empty"
    else
        Ok m

let uniqueStringFieldValidator ctx documentType allowedId fieldName (getter: 'a -> string option) (m: 'a) = task {
    match getter m with
    | None -> return Ok m
    | Some v ->
        let! valid = Database.checkUniqueness documentType fieldName v allowedId ctx
        match valid with
        | true -> return Ok m
        | false -> return Error $"%s{fieldName} must be unique"
}

let handleFileUpload ctx documentType id key (existingPath: string option) (modelSetter: string option -> 'a) = task {
    let files = Util.uploadedFiles ctx
    let file = files |> List.filter (fun f -> f.Name = key) |> List.tryHead

    let! filePath =
        match file with
        | None -> System.Threading.Tasks.Task.FromResult(Ok existingPath)
        | Some file -> Util.saveFileToDataStore file documentType id key ctx

    return match filePath with
           | Error msg -> Error msg
           | Ok coverImagePath -> Ok (modelSetter coverImagePath)
}

let handleImageUpload ctx documentType documentId key (existingPath: Image.ImagePaths option) (modelSetter: Image.ImagePaths option -> 'a) = task {
    let files = Util.uploadedFiles ctx
    let originalFile = files |> List.filter (fun f -> f.Name = key) |> List.tryHead

    match originalFile with
    | None -> return Ok (modelSetter existingPath)
    | Some file -> 
        let! o = Image.saveImageToDataStore file documentType documentId key ctx
        return match o with
               | Error msg -> Error msg
               | Ok newPaths -> Ok (modelSetter (Some newPaths))

}