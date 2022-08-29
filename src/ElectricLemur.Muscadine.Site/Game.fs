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
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "_id")
        isUnique = true})

    let _dateAdded = FormFields.FormField.RequiredDateTimeField({
        Key = "_dateAdded"
        Label = "Date Added"
        getValueFromModel = (fun g -> Some g.DateAdded)
        getValueFromContext = (fun ctx -> None)
        getValueFromJObject = (fun obj -> JObj.getter<System.DateTimeOffset> obj "_dateAdded")
        isUnique = false})

    let name = FormFields.FormField.RequiredStringField ({
        Key = "name"
        Label = "Name"
        getValueFromModel = (fun g -> Some g.Name)
        getValueFromContext = (fun ctx -> HttpFormFields.fromContext ctx |> HttpFormFields.stringOptionValue "name")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "name")
        isUnique = true})

    let description = FormFields.FormField.RequiredStringField ({
        Key = "description"
        Label = "Description"
        getValueFromModel = (fun g -> Some g.Description)
        getValueFromContext = (fun ctx -> HttpFormFields.fromContext ctx |> HttpFormFields.stringOptionValue "description")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "description")
        isUnique = false})

    let slug = FormFields.FormField.RequiredStringField ({
        Key = "slug"
        Label = "Slug"
        getValueFromModel = (fun g -> Some g.Slug)
        getValueFromContext = (fun ctx -> HttpFormFields.fromContext ctx |> HttpFormFields.stringOptionValue "slug")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "slug")
        isUnique = true})
    
    let coverImagePaths = FormFields.FormField.OptionalImagePathsField ({
        Key = "coverImage"
        Label = "Cover Image"
        getValueFromModel = (fun g -> g.CoverImagePaths)
        getValueFromContext = (fun _ -> raise (new NotImplementedException("Cannot get coverImage from form fields")))
        getValueFromJObject = (fun obj -> JObj.getter<Image.ImagePaths> obj "coverImage")
        isUnique = false})

    let viewFields = [
        name; description; slug; coverImagePaths
    ]

let validateModel (id: string) (g: Game) ctx =
    FormFields.validateFieldsOnModel ctx documentType id Fields.viewFields g

let makeAndValidateModelFromContext (existing: Game option) (ctx: HttpContext): Task<Result<Game, string>> =
    let id = existing |> Option.map (fun g -> g.Id) |> Option.defaultValue (string (Util.newGuid ()))
    let dateAdded = existing |> Option.map (fun g -> g.DateAdded) |> Option.defaultValue System.DateTimeOffset.UtcNow

    let httpFormFieldsAreValid = FormFields.validateFieldsOnContext ctx documentType id Fields.viewFields

    match httpFormFieldsAreValid with
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
