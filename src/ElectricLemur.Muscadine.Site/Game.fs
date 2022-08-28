module ElectricLemur.Muscadine.Site.Game
open Giraffe
open Giraffe.ViewEngine
open System
open Microsoft.AspNetCore.Http
open System.Threading.Tasks
open Newtonsoft.Json.Linq
open RequiredFields
open OptionalFields

let documentType = Constants.Database.DocumentTypes.Game

type Game = {
    Id: string;
    DateAdded: DateTimeOffset;
    Name: string;
    Description: string;
    Slug: string;
    CoverImagePaths: Image.ImagePaths option;
}

module Fields =

    let _id = FormFields.FormField.RequiredStringField ({
        Key = "_id"
        Label = "Id"
        getValueFromModel = (fun g -> Some g.Id)
        getValueFromContext = (fun ctx -> None)
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "_id") })

    let _dateAdded = FormFields.FormField.RequiredDateTimeField({
        Key = "_dateAdded"
        Label = "Date Added"
        getValueFromModel = (fun g -> Some g.DateAdded)
        getValueFromContext = (fun ctx -> None)
        getValueFromJObject = (fun obj -> JObj.getter<System.DateTimeOffset> obj "_dateAdded") })

    let name = FormFields.FormField.RequiredStringField ({
        Key = "name"
        Label = "Name"
        getValueFromModel = (fun g -> Some g.Name)
        getValueFromContext = (fun ctx -> HttpFormFields.fromContext ctx |> HttpFormFields.stringOptionValue "name")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "name") })

    let description = FormFields.FormField.RequiredStringField ({
        Key = "description"
        Label = "Description"
        getValueFromModel = (fun g -> Some g.Description)
        getValueFromContext = (fun ctx -> HttpFormFields.fromContext ctx |> HttpFormFields.stringOptionValue "description")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "description") })

    let slug = FormFields.FormField.RequiredStringField ({
        Key = "slug"
        Label = "Slug"
        getValueFromModel = (fun g -> Some g.Slug)
        getValueFromContext = (fun ctx -> HttpFormFields.fromContext ctx |> HttpFormFields.stringOptionValue "slug")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "slug") })
    
    let coverImagePaths = FormFields.FormField.OptionalImagePathsField ({
        Key = "coverImage"
        Label = "Cover Image"
        getValueFromModel = (fun g -> g.CoverImagePaths)
        getValueFromContext = (fun _ -> raise (new NotImplementedException("Cannot get coverImage from form fields")))
        getValueFromJObject = (fun obj -> JObj.getter<Image.ImagePaths> obj "coverImage" )})

    let viewFields = [
        name; description; slug; coverImagePaths
    ]
let addEditView (g: Game option) fields allTags documentTags =

    let pageTitle = match g with
                    | None -> "Add Game" 
                    | Some g-> $"Edit Game %s{g.Name}"

    let pageData =
        match g with
        | Some g -> Map.ofList [ ("id", Items.pageDataType.String g.Id); ("slug", Items.pageDataType.String  "game")]
        | None -> Map.empty


    Items.layout pageTitle pageData [
        div [ _class "page-title" ] [ encodedText pageTitle ]
        form [ _name "game-form"; _method "post"; _enctype "multipart/form-data" ] [
                table [] [
                    for ff in fields do
                        yield FormFields.View.makeFormFieldRow ff g

                    yield Items.makeTagsInputRow "Tags" Tag.formKey allTags documentTags
                    yield tr [] [
                        td [] []
                        td [] [ input [ _type "submit"; _value "Save" ] ]
                    ]
                ]
            ]

        (match g with
        | None -> Util.emptyDiv
        | Some _ ->
            div [ _class "microblog-container section" ] [])
    ]

let validateModel (id: string) (g: Game) ctx =
    let stringFieldUniquenessValidator field = FormFields.stringFieldUniquenessValidator ctx documentType id field

    Ok g
    |> Task.fromResult
    |> Task.map (Items.performValidation (FormFields.requiredStringValidator Fields.name))
    |> Task.map (Items.performValidation (FormFields.requiredStringValidator Fields.description))
    |> Task.map (Items.performValidation (FormFields.requiredStringValidator Fields.slug))
    |> Task.bind (Items.performValidationAsync (stringFieldUniquenessValidator Fields.name))
    |> Task.bind (Items.performValidationAsync (stringFieldUniquenessValidator Fields.slug))


let makeAndValidateModelFromContext (existing: Game option) (ctx: HttpContext): Task<Result<Game, string>> =
    let id = existing |> Option.map (fun g -> g.Id) |> Option.defaultValue (string (Util.newGuid ()))
    let dateAdded = existing |> Option.map (fun g -> g.DateAdded) |> Option.defaultValue System.DateTimeOffset.UtcNow

    let fields = HttpFormFields.fromContext ctx
    let requiredFieldsAreValid = 
        Ok ()
        |> HttpFormFields.checkRequiredStringField fields (FormFields.key Fields.name)
        |> HttpFormFields.checkRequiredStringField fields (FormFields.key Fields.description)
        |> HttpFormFields.checkRequiredStringField fields (FormFields.key Fields.slug)

    match requiredFieldsAreValid with
    | Ok _ ->
        let getFormStringValue f = FormFields.ContextValue.string f ctx |> Option.get
        let getModelImagePathsValue f =
            existing |> Option.bind (fun g -> FormFields.ModelValue.imagePaths f g)

        let g = {
            Id =            id
            DateAdded =     dateAdded
            Name =          Fields.name |> getFormStringValue
            Description =   Fields.description |> getFormStringValue
            Slug =          Fields.slug |> getFormStringValue
            CoverImagePaths = Fields.coverImagePaths |> getModelImagePathsValue
        }
        validateModel id g ctx
    | Error msg -> Task.fromResult (Error msg)


let makeModelFromJObject (obj: JObject) =
    let stringValue f = FormFields.DatabaseValue.string f obj |> Option.get
    let dateValue f = FormFields.DatabaseValue.dateTime f obj |> Option.get
    let getImagePathsValue f = FormFields.DatabaseValue.imagePaths f obj

    {
        Id =            Fields._id |> stringValue
        DateAdded =     Fields._dateAdded |> dateValue
        Name =          Fields.name |> stringValue
        Description =   Fields.description |> stringValue
        Slug =          Fields.slug |> stringValue
        CoverImagePaths = Fields.coverImagePaths |> getImagePathsValue
    }

let tryMakeModelFromJObject (obj: JObject) =
    let objectType = Database.documentTypeField |> JObj.getter<string> obj
    match objectType with
    | Some o when o = documentType ->
        try
            Some (makeModelFromJObject obj)
        with
        | _ -> None
    | _ -> None

let makeJObjectFromModel (g: Game) =
    (new JObject())
    |> (fun obj ->
            obj.["_documentType"] <- documentType
            obj)
    |> FormFields.setJObject g Fields._id
    |> FormFields.setJObject g Fields._dateAdded
    |> FormFields.setJObject g Fields.name
    |> FormFields.setJObject g Fields.description
    |> FormFields.setJObject g Fields.slug
    |> FormFields.setJObject g Fields.coverImagePaths
