module ElectricLemur.Muscadine.Site.Project
open Giraffe
open Giraffe.ViewEngine
open System
open Microsoft.AspNetCore.Http
open System.Threading.Tasks
open Newtonsoft.Json.Linq
open RequiredFields
open OptionalFields

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
    let _id: RequiredFieldDescriptor<Project, string> = {
        Key = "_id"
        Label = "Id"
        getValueFromModel = (fun p -> p.Id)
        getValueFromContext = (fun ctx -> None)
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "_id" |> Option.get) }

    let _dateAdded: RequiredFieldDescriptor<Project, System.DateTimeOffset> = {
        Key = "_dateAdded"
        Label = "Date Added"
        getValueFromModel = (fun p -> p.DateAdded)
        getValueFromContext = (fun ctx -> None)
        getValueFromJObject = (fun obj -> JObj.getter<System.DateTimeOffset> obj "_dateAdded" |> Option.get) }

    let name: RequiredFieldDescriptor<Project, string> = {
        Key = "name"
        Label = "Name"
        getValueFromModel = (fun p -> p.Name)
        getValueFromContext = (fun ctx -> FormFields.fromContext ctx |> FormFields.stringOptionValue "name")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "name" |> Option.get) }

    let description: RequiredFieldDescriptor<Project, string> = {
        Key = "description"
        Label = "Description"
        getValueFromModel = (fun p -> p.Description)
        getValueFromContext = (fun ctx -> FormFields.fromContext ctx |> FormFields.stringOptionValue "description")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "description" |> Option.get) }

    let slug: RequiredFieldDescriptor<Project, string> = {
        Key = "slug"
        Label = "Slug"
        getValueFromModel = (fun p -> p.Slug)
        getValueFromContext = (fun ctx -> FormFields.fromContext ctx |> FormFields.stringOptionValue "slug")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "slug" |> Option.get) }
    
    let coverImagePaths: OptionalFieldDescriptor<Project, Image.ImagePaths> = {
        Key = "coverImage"
        Label = "Icon Image"
        getValueFromModel = (fun p -> p.IconImagePaths)
        getValueFromContext = (fun _ -> raise (new NotImplementedException("Cannot get coverImage from form fields")))
        getValueFromJObject = (fun obj -> JObj.getter<Image.ImagePaths> obj "coverImage")
    }

    let gitHubLink: OptionalFieldDescriptor<Project, string> = {
        Key = "githubLink"
        Label = "Github Link"
        getValueFromModel = (fun p -> p.GitHubLink)
        getValueFromContext = (fun ctx -> FormFields.fromContext ctx |> FormFields.stringOptionValue "githubLink")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "githubLink")
    }

let addEditView (p: Project option) allTags documentTags =

    let pageTitle = match p with
                    | None -> "Add Project" 
                    | Some p-> $"Edit Project %s{p.Name}"
    let pageData =
        match p with
        | Some p -> Map.ofList [ ("id", Items.pageDataType.String p.Id); ("slug", Items.pageDataType.String  "project")]
        | None -> Map.empty

    let makeTextRow ff =
        let v = p |> Option.map (RequiredFields.modelGetter ff)
        Items.makeTextInputRow (RequiredFields.label ff) (RequiredFields.key ff) v

    let makeTextAreaRow ff lines =
        let v = p |> Option.map (RequiredFields.modelGetter ff)
        Items.makeTextAreaInputRow (RequiredFields.label ff) (RequiredFields.key ff) lines v

    let makeOptionalTextRow ff =
        let v = p |> Option.map (OptionalFields.modelGetter ff) |> Option.flatten
        Items.makeTextInputRow (OptionalFields.label ff) (OptionalFields.key ff) v

    let makecheckboxRow ff = 
        let v = p |> Option.map (RequiredFields.modelGetter ff)
        Items.makeCheckboxInputRow (RequiredFields.label ff) (RequiredFields.key ff) v

    let makeImageRow (ff: OptionalFields.OptionalFieldDescriptor<Project, Image.ImagePaths>) =
        let v = p 
                |> Option.map (OptionalFields.modelGetter ff) 
                |> Option.flatten
                |> Option.map (fun paths -> paths.Size512)

        Items.makeImageInputRow (OptionalFields.label ff) (OptionalFields.key ff) v

    Items.layout pageTitle pageData [
        div [ _class "page-title" ] [ encodedText pageTitle ]
        form [ _name "book-form"; _method "post"; _enctype "multipart/form-data" ] [
                table [] [
                    makeTextRow Fields.name
                    makeTextAreaRow Fields.description 5
                    makeOptionalTextRow Fields.gitHubLink
                    makeTextRow Fields.slug
                    makeImageRow Fields.coverImagePaths
                    Items.makeTagsInputRow "Tags" Tag.formKey allTags documentTags

                    tr [] [
                        td [] []
                        td [] [ input [ _type "submit"; _value "Save" ] ]
                    ]
                ]
            ]

        (match p with
        | None -> Util.emptyDiv
        | Some _ ->
            div [ _class "microblog-container section" ] [])
    ]

let validateModel (id: string) (g: Project) ctx =
    let stringFieldUniquenessValidator field = RequiredFields.stringFieldUniquenessValidator ctx documentType id field

    Ok g
    |> Task.fromResult
    |> Task.map (Items.performValidation (RequiredFields.requiredStringValidator Fields.name))
    |> Task.map (Items.performValidation (RequiredFields.requiredStringValidator Fields.description))
    |> Task.map (Items.performValidation (RequiredFields.requiredStringValidator Fields.slug))
    |> Task.bind (Items.performValidationAsync (stringFieldUniquenessValidator Fields.name))
    |> Task.bind (Items.performValidationAsync (stringFieldUniquenessValidator Fields.slug))


let makeAndValidateModelFromContext (existing: Project option) (ctx: HttpContext): Task<Result<Project, string>> =
    let id = existing |> Option.map (fun p -> p.Id) |> Option.defaultValue (string (Util.newGuid ()))
    let dateAdded = existing |> Option.map (fun p -> p.DateAdded) |> Option.defaultValue System.DateTimeOffset.UtcNow

    let fields = FormFields.fromContext ctx
    let requiredFieldsAreValid = 
        Ok ()
        |> FormFields.checkRequiredStringField fields (RequiredFields.key Fields.name)
        |> FormFields.checkRequiredStringField fields (RequiredFields.key Fields.description)
        |> FormFields.checkRequiredStringField fields (RequiredFields.key Fields.slug)
    
    let o = ctx |> (RequiredFields.formGetter Fields.name) |> Option.get
    match requiredFieldsAreValid with
    | Ok _ -> 
        let getValue f = (f |> RequiredFields.formGetter) ctx |> Option.get
        let getOptionalValue f = (f |> OptionalFields.formGetter) ctx
        let getNonFormFieldValue f = existing |> Option.map (fun g -> (f |> OptionalFields.modelGetter) g) |> Option.flatten

        let g = {
            Id =            id
            DateAdded =     dateAdded
            Name =         Fields.name |> getValue
            Description =   Fields.description |> getValue
            Slug =          Fields.slug |> getValue
            IconImagePaths = Fields.coverImagePaths |> getNonFormFieldValue
            GitHubLink = Fields.gitHubLink |> getOptionalValue
        }
        validateModel id g ctx
    | Error msg -> Task.fromResult (Error msg)


let makeModelFromJObject (obj: JObject) =
    let getValue f = (f |> RequiredFields.jobjGetter) obj
    let getOptionalValue f = (f |> OptionalFields.jobjGetter) obj

    {
        Id =            Fields._id |> getValue
        DateAdded =     Fields._dateAdded |> getValue
        Name =         Fields.name |> getValue
        Description =   Fields.description |> getValue
        Slug =          Fields.slug |> getValue
        IconImagePaths = Fields.coverImagePaths |> getOptionalValue
        GitHubLink = Fields.gitHubLink |> getOptionalValue
    }

let makeJObjectFromModel (p: Project) =
    (new JObject())
    |> (fun obj -> 
            obj.["_documentType"] <- documentType
            obj)
    |> RequiredFields.setJObject p Fields._id
    |> RequiredFields.setJObject p Fields._dateAdded
    |> RequiredFields.setJObject p Fields.name
    |> RequiredFields.setJObject p Fields.description
    |> RequiredFields.setJObject p Fields.slug
    |> OptionalFields.setJObject p Fields.coverImagePaths
    |> OptionalFields.setJObject p Fields.gitHubLink
