module ElectricLemur.Muscadine.Site.Project
open System
open Microsoft.AspNetCore.Http
open System.Threading.Tasks
open Newtonsoft.Json.Linq

let documentType = Constants.Database.DocumentTypes.Project

type Project = {
    Id: string;
    DateAdded: DateTimeOffset;
    Name: string;
    Description: string;
    Slug: string;
    IconImagePaths: Image.ImagePaths option;
    GitHubLink: string option;
}

module Fields = 
    let _id = FormFields.FormField.RequiredStringField ({
        Key = "_id"
        Label = "Id"
        getValueFromModel = (fun p -> Some p.Id)
        getValueFromContext = (fun ctx -> None)
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "_id")
        isUnique = true})

    let _dateAdded = FormFields.FormField.RequiredDateTimeField({
        Key = "_dateAdded"
        Label = "Date Added"
        getValueFromModel = (fun p -> Some p.DateAdded)
        getValueFromContext = (fun ctx -> None)
        getValueFromJObject = (fun obj -> JObj.getter<System.DateTimeOffset> obj "_dateAdded")
        isUnique = false})

    let name = FormFields.FormField.RequiredStringField ({
        Key = "name"
        Label = "Name"
        getValueFromModel = (fun p -> Some p.Name)
        getValueFromContext = (fun ctx -> HttpFormFields.fromContext ctx |> HttpFormFields.stringOptionValue "name")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "name")
        isUnique = false})

    let description = FormFields.FormField.RequiredStringField ({
        Key = "description"
        Label = "Description"
        getValueFromModel = (fun p -> Some p.Description)
        getValueFromContext = (fun ctx -> HttpFormFields.fromContext ctx |> HttpFormFields.stringOptionValue "description")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "description")
        isUnique = false})

    let slug = FormFields.FormField.RequiredStringField ({
        Key = "slug"
        Label = "Slug"
        getValueFromModel = (fun p -> Some p.Slug)
        getValueFromContext = (fun ctx -> HttpFormFields.fromContext ctx |> HttpFormFields.stringOptionValue "slug")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "slug")
        isUnique = true})
    
    let coverImagePaths = FormFields.FormField.OptionalImagePathsField ({
        Key = "coverImage"
        Label = "Icon Image"
        getValueFromModel = (fun p -> p.IconImagePaths)
        getValueFromContext = (fun _ -> raise (new NotImplementedException("Cannot get coverImage from form fields")))
        getValueFromJObject = (fun obj -> JObj.getter<Image.ImagePaths> obj "coverImage")
        isUnique = false})

    let gitHubLink = FormFields.FormField.OptionalStringField({
        Key = "githubLink"
        Label = "Github Link"
        getValueFromModel = (fun p -> p.GitHubLink)
        getValueFromContext = (fun ctx -> HttpFormFields.fromContext ctx |> HttpFormFields.stringOptionValue "githubLink")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "githubLink")
        isUnique = false})

    let allFields = [ _id; _dateAdded; name; description; slug; coverImagePaths; gitHubLink ]

    let viewFields = allFields |> List.filter(fun f ->
        (FormFields.key f) <> (FormFields.key _id) &&
        (FormFields.key f) <> (FormFields.key _dateAdded))


let validateModel (id: string) (g: Project) ctx =
    FormFields.validateFieldsOnModel ctx documentType id Fields.viewFields g


let makeAndValidateModelFromContext (existing: Project option) (ctx: HttpContext): Task<Result<Project, string>> =
    let id = existing |> Option.map (fun p -> p.Id) |> Option.defaultValue (string (Util.newGuid ()))
    let dateAdded = existing |> Option.map (fun p -> p.DateAdded) |> Option.defaultValue System.DateTimeOffset.UtcNow

    let httpFormFieldsAreValid = FormFields.validateFieldsOnContext ctx documentType id Fields.viewFields

    match httpFormFieldsAreValid with
    | Ok _ ->
        let getFormStringValue f = FormFields.ContextValue.string f ctx |> Option.get
        let getOptionalFormStringValue f = FormFields.ContextValue.string f ctx
        let getModelImagePathsValue f =
            existing |> Option.bind (fun g -> FormFields.ModelValue.imagePaths f g)

        let g = {
            Id =            id
            DateAdded =     dateAdded
            Name =         Fields.name |> getFormStringValue
            Description =   Fields.description |> getFormStringValue
            Slug =          Fields.slug |> getFormStringValue
            IconImagePaths = Fields.coverImagePaths |> getModelImagePathsValue
            GitHubLink = Fields.gitHubLink |> getOptionalFormStringValue
        }
        validateModel id g ctx
    | Error msg -> Task.fromResult (Error msg)


let makeModelFromJObject (obj: JObject) =
    let stringValue f = FormFields.DatabaseValue.string f obj |> Option.get
    let optionalStringValue f = FormFields.DatabaseValue.string f obj
    let dateValue f = FormFields.DatabaseValue.dateTime f obj |> Option.get
    let getImagePathsValue f = FormFields.DatabaseValue.imagePaths f obj

    {
        Id =            Fields._id |> stringValue
        DateAdded =     Fields._dateAdded |> dateValue
        Name =         Fields.name |> stringValue
        Description =   Fields.description |> stringValue
        Slug =          Fields.slug |> stringValue
        IconImagePaths = Fields.coverImagePaths |> getImagePathsValue
        GitHubLink = Fields.gitHubLink |> optionalStringValue
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
