module ElectricLemur.Muscadine.Site.Book
open System
open Microsoft.AspNetCore.Http
open System.Threading.Tasks
open Newtonsoft.Json.Linq

let documentType = Constants.Database.DocumentTypes.Book

type Book = {
    Id: string;
    DateAdded: DateTimeOffset;
    Title: string;
    Description: string;
    Slug: string;
    CoverImagePaths: Image.ImagePaths option;
}

module Fields =
    let _id = FormFields.FormField.RequiredStringField ({
        Key = "_id"
        Label = "Id"
        getValueFromModel = (fun b -> Some b.Id)
        getValueFromContext = (fun ctx -> None)
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "_id")
        isUnique = true})

    let _dateAdded = FormFields.FormField.RequiredDateTimeField ({
        Key = "_dateAdded"
        Label = "Date Added"
        getValueFromModel = (fun b -> Some b.DateAdded)
        getValueFromContext = (fun ctx -> None)
        getValueFromJObject = (fun obj -> JObj.getter<System.DateTimeOffset> obj "_dateAdded")
        isUnique = false})

    let title = FormFields.FormField.RequiredStringField ({
        Key = "name"
        Label = "Name"
        getValueFromModel = (fun b -> Some b.Title)
        getValueFromContext = (fun ctx -> HttpFormFields.fromContext ctx |> HttpFormFields.stringOptionValue "name")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "name")
        isUnique = false})

    let description = FormFields.FormField.RequiredStringField ({
        Key = "description"
        Label = "Description"
        getValueFromModel = (fun b -> Some b.Description)
        getValueFromContext = (fun ctx -> HttpFormFields.fromContext ctx |> HttpFormFields.stringOptionValue "description")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "description")
        isUnique = false})

    let slug = FormFields.FormField.RequiredStringField ({
        Key = "slug"
        Label = "Slug"
        getValueFromModel = (fun b -> Some b.Slug)
        getValueFromContext = (fun ctx -> HttpFormFields.fromContext ctx |> HttpFormFields.stringOptionValue "slug")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "slug")
        isUnique = true})
    
    let coverImagePaths = FormFields.FormField.OptionalImagePathsField({
        Key = "coverImage"
        Label = "Cover Image"
        getValueFromModel = (fun b -> b.CoverImagePaths)
        getValueFromContext = (fun _ -> raise (new NotImplementedException("Cannot get coverImage from form fields")))
        getValueFromJObject = (fun obj -> JObj.getter<Image.ImagePaths> obj "coverImage")
        isUnique = false})

    let allFields = [ _id; _dateAdded; title; description; slug; coverImagePaths ]

let validateModel (id: string) (b: Book) ctx =
    FormFields.validateFieldsOnModel ctx documentType id Fields.allFields b

let makeAndValidateModelFromContext (existing: Book option) (ctx: HttpContext): Task<Result<Book, string>> =
    let id = existing |> Option.map (fun g -> g.Id) |> Option.defaultValue (string (Util.newGuid ()))
    let dateAdded = existing |> Option.map (fun g -> g.DateAdded) |> Option.defaultValue System.DateTimeOffset.UtcNow

    let httpFormFieldsAreValid = FormFields.validateFieldsOnContext ctx documentType id Fields.allFields

    match httpFormFieldsAreValid with
    | Ok _ -> 
        let getFormStringValue f = FormFields.ContextValue.string f ctx |> Option.get
        let getModelImagePathsValue f =
            existing |> Option.bind (fun g -> FormFields.ModelValue.imagePaths f g)

        let g = {
            Id =            id
            DateAdded =     dateAdded
            Title =         Fields.title |> getFormStringValue
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
        Title =         Fields.title|> stringValue
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
