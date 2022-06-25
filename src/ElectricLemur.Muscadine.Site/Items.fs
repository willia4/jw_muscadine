module ElectricLemur.Muscadine.Site.Items

open Microsoft.AspNetCore.Http
open System.Threading.Tasks
open Giraffe.ViewEngine

type FieldType =
    /// The field is represented in the UI by a text box and backed by a string or string option in the model
    | TextField
    /// The field is represented in the UI by a check box and backed by a bool or bool option in the model
    | CheckboxField
    /// The field is represented in the UI by a date field and backed by a DateTimeOffset or DateTimeOffset option in the model
    | DateTime
type RequiredField =
    /// The field is not wrapped by an option in the model
    | Required
    /// The field is represented in the model as an Option<'a>
    | NotRequired

type Field = {
    Name: string;
    FieldType: FieldType;
    Required: RequiredField;
}

module Helpers =
    open Microsoft.FSharp.Reflection
    open Newtonsoft.Json.Linq

    let private getModelFieldType (f: System.Reflection.PropertyInfo) =
        let stringType = "".GetType()
        let stringOptionType = (Some "").GetType()
        let boolType = true.GetType()
        let boolOptionType = (Some true).GetType()
        let dateTimeType = System.DateTimeOffset.UtcNow.GetType()
        let dateTimeOptionType = (Some System.DateTimeOffset.UtcNow).GetType()

        match f.PropertyType with
        | t when t = stringType -> (TextField, Required)
        | t when t = stringOptionType -> (TextField, NotRequired)
        | t when t = boolType -> (CheckboxField, Required)
        | t when t = boolOptionType -> (CheckboxField, NotRequired)
        | t when t = dateTimeType -> (DateTime, Required)
        | t when t = dateTimeOptionType -> (DateTime, NotRequired)
        | _ -> failwith $"Unknown record field type type %s{f.PropertyType.FullName}"

    let private modelFields<'a> = 
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
            let (t, req) = getModelFieldType f
            {
                Name = f.Name
                FieldType = t
                Required = req
            }
        )
    
    let databaseFields = [ 
        { Name = "_id"; FieldType = TextField; Required = Required}; 
        { Name = "_dateAdded"; FieldType = DateTime; Required = Required};
    ]


    let private getFieldValueFromJObject (obj: JObject) fieldType : obj =
        match (fieldType.FieldType, fieldType.Required, fieldType.Name) with
        | (TextField, Required, n) -> n |> JObj.getter<string> obj |> Option.get |> box
            
        | (TextField, NotRequired, n) -> n |> JObj.getter<string> obj |> box

        | (CheckboxField, Required, n) -> n |> JObj.getter<bool> obj |> Option.get |> box
        | (CheckboxField, NotRequired, n) -> n |> JObj.getter<bool> obj |> box

        | (DateTime, Required, n) -> n |> JObj.getter<System.DateTimeOffset> obj |> Option.get |> box

        | (DateTime, NotRequired, n) -> n |> JObj.getter<System.DateTimeOffset> obj |> box
            

    /// Converts a JObject, obj, to a model object
    /// Throws an exception if obj does not contain all required fields for the model
    /// Throws an exception if the type 'a does not conform to expectations provided by the Field type
    let makeModelFromJObject<'a> (obj: JObject) =
        let fieldTypes = modelFields<'a>
        let requiredFields = fieldTypes |> Seq.filter (fun f -> f.Required = Required) |> Seq.append databaseFields // JObjects come from the database so need the DB fields
        let fieldExists f = JObj.fieldExists obj f.Name
        let missing = requiredFields |> Seq.filter (fieldExists >> not)

        if missing |> Seq.isEmpty then
            let constructorValues =
                fieldTypes
                |> Seq.append databaseFields
                |> Seq.map (getFieldValueFromJObject obj)
                |> Seq.toArray
            let m = FSharpValue.MakeRecord(typedefof<'a>, constructorValues)
            m :?> 'a
        else
            let missing = System.String.Join(",", (missing |> Seq.map (fun f -> f.Name)))
            failwith $"JObject missing fields [%s{missing}] for model type %s{(typedefof<'a>).FullName}"

    /// Converts a model object, m, to a JObject. 
    /// Throws an exception if the model object does not conform to expectations provided by the Field type
    let makeJObjectFromModel (m: 'a) (documentType: string) =
        let fieldTypes = modelFields<'a> |> Seq.append databaseFields // JObjects are saved to the database so need the DB fields
        let values = FSharpValue.GetRecordFields(m)

        let r = 
            Seq.zip fieldTypes values 
            |> Seq.fold (fun (acc: JObject) (fieldType, v) -> 
                match (fieldType.FieldType, fieldType.Required, fieldType.Name) with
                    | (TextField, Required, n) -> acc.[n] <- (v :?> string)
                    | (TextField, NotRequired, n) -> 
                        let v = v :?> string option
                        match v with
                        | Some v -> acc.[n] <- v
                        | _ -> ()
                    | (CheckboxField, Required, n) -> acc.[n] <- (v :?> bool)
                    | (CheckboxField, NotRequired, n) -> 
                        let v = v :?> bool option
                        match v with
                        | Some v -> acc.[n] <- v
                        | _ -> ()

                    | (DateTime, Required, n) -> acc.[n] <- (v :?> System.DateTimeOffset).ToString("o")
                    | (DateTime, NotRequired, n) ->
                        let v = v :?> System.DateTimeOffset option
                        match v with
                        | Some v -> acc.[n] <- v.ToString("o")
                        | _ -> ()
                acc) 
                (new JObject())

        r.["_documentType"] <- documentType
        r

    /// Attempts to convert a form data set to a model object. 
    /// Throws an exception if the type 'a does not conform to expectations provided by the Field type
    /// Returns an Error result if data is missing or cannot be converted from the form data set
    let makeModelFromFormFields<'a> (id: string) (dateAdded: System.DateTimeOffset) (form: IFormCollection) : Result<'a, string> =
        let fieldTypes = modelFields<'a>
        let requiredFields = fieldTypes |> Seq.filter (fun f -> f.Required = Required)
        let fieldExists f = form.ContainsKey(f.Name)
        let missing = requiredFields |> Seq.filter (fieldExists >> not) 

        let extractFieldValue fieldType : obj option =
            if form.ContainsKey(fieldType.Name) then
                let s = box(form.[fieldType.Name])
                let s = if s = null then None else Some (string s)

                match (fieldType.FieldType, fieldType.Required) with
                | TextField, Required ->
                    match s with
                    | Some s -> Some (box s)
                    | None -> None
                | TextField, NotRequired ->
                    match s with
                    | Some s -> Some (Some (box s))
                    | None -> Some (None)
                | CheckboxField, Required ->
                    match s with
                    | Some s when s = "on" || s = "true" -> Some (box true)
                    | Some s when s = "false" -> Some (box false)
                    | _ -> Some (box false)
                | CheckboxField, NotRequired ->
                    match s with
                    | Some s when s = "on" || s = "true" -> Some (Some (box true))
                    | Some s when s = "false" -> Some (Some (box false))
                    | _ -> Some (Some (box false))
                | DateTime, Required ->
                    match s with
                    | Some s ->
                        match System.DateTimeOffset.TryParse(s) with
                        | true, d -> Some (box s)
                        | false, _ -> None
                    | None -> None
                | DateTime, NotRequired ->
                    match s with
                    | Some s ->
                        match System.DateTimeOffset.TryParse(s) with
                        | true, d -> Some (Some (box d))
                        | false, _ -> Some (None)
                    | None -> Some (None)
            else
                None

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

    let modelFieldType<'a> fieldName = 
        let fields = modelFields<'a>
        let field = fields |> Seq.find (fun f -> f.Name = fieldName)
        (field.FieldType, field.Required)

    let modelObjValue (m: 'a) (fieldName: string) =
        let fieldInfos = FSharpType.GetRecordFields(typedefof<'a>)
        let fieldInfo = fieldInfos |> Array.find (fun i -> i.Name = fieldName)
        FSharpValue.GetRecordField(m, fieldInfo)

    let modelStringValue (m: 'a) (fieldName: string) =
        let value = modelObjValue m fieldName
        match value with
        | null -> None
        | _ -> Some (string value)

    let validateRequiredFields (prev: Result<'a, string>) =
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

        let validateRequiredField (prev: Result<'a, string>) f isEmpty =
            match prev with
            | Error s -> Error s
            | Ok m -> 
                let fieldValue = modelObjValue m f.Name
                if isEmpty fieldValue then 
                    Error $"%s{f.Name} cannot be empty"
                else
                    Ok m

        match prev with
        | Error s -> Error s
        | Ok m -> 
            let fieldTypes = modelFields<'a>
            let requiredFields = fieldTypes |> Seq.filter (fun f -> f.Required = Required)
            requiredFields 
            |> Seq.fold (fun prev f ->
                match f.FieldType with
                | TextField -> validateRequiredField prev f stringFieldIsEmpty
                | CheckboxField -> validateRequiredField prev f boolFieldIsEmpty
                | DateTime -> validateRequiredField prev f dateFieldIsEmpty
            ) (Ok m)



    let performValidation (f: 'a -> Task<Result<'a, string>>) (prev: Result<'a, string>) = 
        match prev with
        | Error s -> Task.FromResult(Error s)
        | Ok g -> task {
            return! f g
        }

let layout pageTitle content =
    html [] [
        head [] [
            title [] [ encodedText pageTitle ]
            link [ (_rel "stylesheet"); (_type "text/css"); (_href "/css/admin.css") ]
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

let makeTextInputRow model label key value = 
    let el = input [ 
        _type "text"
        _id key
        _name key
        _value (value |> Option.defaultValue "")
    ]
    makeInputRow label el

let makeInputRowForFormElement (model: 'a option) (label: string) key =
    let (fieldType, req) = Helpers.modelFieldType<'a> key
    let label = if req = Required then
                    if label.EndsWith("*") then label else $"%s{label}*"
                else
                    label

    let stringValue key = model |> Option.map (fun m -> Helpers.modelStringValue m key) |> Option.flatten

    match fieldType with
    | TextField -> makeTextInputRow model label key (stringValue key)
    | DateTime -> failwith "Unsupported field type, DateTime"
    | CheckboxField -> failwith "Unsupported field type, CheckboxField" //TODO