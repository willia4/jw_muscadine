module ElectricLemur.Muscadine.Site.Project
open Giraffe
open Giraffe.ViewEngine
open System
open Microsoft.AspNetCore.Http
open System.Threading.Tasks
open Newtonsoft.Json.Linq
open RequiredFields
open OptionalFields

let documentType = "project"

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

    let makeTextRow ff = 
        let v = p |> Option.map (RequiredFields.modelGetter ff) 
        Items.makeTextInputRow (RequiredFields.label ff) (RequiredFields.key ff) v

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

    Items.layout pageTitle [
        div [ _class "page-title" ] [ encodedText pageTitle ]
        form [ _name "book-form"; _method "post"; _enctype "multipart/form-data" ] [
                table [] [
                    makeTextRow Fields.name
                    makeTextRow Fields.description
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
    ]

let validateModel (id: string) (g: Project) ctx = task {
    let stringFieldUniquenessValidator field = RequiredFields.stringFieldUniquenessValidator ctx documentType id field

    let valid = 
        Ok g
        |> Items.performValidation (RequiredFields.requiredStringValidator Fields.name)
        |> Items.performValidation (RequiredFields.requiredStringValidator Fields.description)
        |> Items.performValidation (RequiredFields.requiredStringValidator Fields.slug)
    let! valid = valid |> Items.performValidationAsync (stringFieldUniquenessValidator Fields.name)
    let! valid = valid |> Items.performValidationAsync (stringFieldUniquenessValidator Fields.slug)

    return valid
}

let makeAndValidateModelFromContext (existing: Project option) (ctx: HttpContext): Task<Result<Project, string>> = task {
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
        return! validateModel id g ctx
    | Error msg -> return Error msg
}

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


let addHandler_get =
    fun next (ctx: HttpContext) -> task {
        let! allTags = Tag.getExistingTags ctx
        return! htmlView (addEditView None allTags []) next ctx
    }
        

let addHandler_post : HttpHandler = 
    fun next (ctx: HttpContext) -> task {
        let! p = makeAndValidateModelFromContext None ctx
        
        match p with
        | Ok p ->
            let! coverImageUploadResult =
                Items.handleImageUpload ctx documentType p.Id Fields.coverImagePaths.Key p.IconImagePaths (fun newPaths -> { p with IconImagePaths = newPaths})

            match coverImageUploadResult with
            | Error msg -> return! (setStatusCode 400 >=> text msg) next ctx
            | Ok p ->
                let data = makeJObjectFromModel p
                let! id = Database.insertDocument ctx data
                do! Tag.saveTagsForForm documentType id Tag.formKey ctx

                return! (redirectTo false $"/admin/project/%s{id}") next ctx
        | Error msg -> return! (setStatusCode 400 >=> text msg) next ctx
    }

let editHandler_get id =
    fun next (ctx: HttpContext) -> task {
        let! existing = Database.getDocumentById id ctx
        let existing = existing |> Option.map makeModelFromJObject

        let! allTags = Tag.getExistingTags ctx
        let! documentTags = Tag.loadTagsForDocument documentType id ctx

        return! htmlView (addEditView existing allTags documentTags) next ctx
    }

let editHandler_post id : HttpHandler =
    fun next (ctx: HttpContext) -> task {
        let! existing = Database.getDocumentById id ctx
        match existing with
        | None -> return! (setStatusCode 404) next ctx
        | Some existing ->
            let existing = makeModelFromJObject existing
            let! p = makeAndValidateModelFromContext (Some existing) ctx
            match p with
            | Ok p ->
                let! coverImageUploadResult = 
                    Items.handleImageUpload ctx documentType p.Id Fields.coverImagePaths.Key p.IconImagePaths (fun newPaths -> { p with IconImagePaths = newPaths})

                match coverImageUploadResult with
                | Error msg -> return! (setStatusCode 400 >=> text msg) next ctx
                | Ok p -> 
                    let data = makeJObjectFromModel p
                    do! Database.upsertDocument ctx data
                    do! Tag.saveTagsForForm documentType p.Id Tag.formKey ctx

                    return! (redirectTo false $"/admin/project/%s{id}") next ctx
            | Error msg -> return! (setStatusCode 400 >=> text msg) next ctx
    }

let deleteHandler_delete id =
    fun next ctx -> task {
        let! existing = Database.getDocumentById id ctx
        let existing = existing |> Option.map makeModelFromJObject
        let existingCoverImage = existing |> Option.map (fun e -> e.IconImagePaths) |> Option.flatten

        match existingCoverImage with
        | Some existingCoverImage ->
            Util.deleteRelativePathIfExists existingCoverImage.Original ctx
            Util.deleteRelativePathIfExists existingCoverImage.Size1024 ctx
            Util.deleteRelativePathIfExists existingCoverImage.Size512 ctx
            Util.deleteRelativePathIfExists existingCoverImage.Size256 ctx
            Util.deleteRelativePathIfExists existingCoverImage.Size128 ctx
            Util.deleteRelativePathIfExists existingCoverImage.Size64 ctx
        | None -> ()

        do! Database.deleteDocument ctx id
        return! setStatusCode 200 next ctx
    }
    
let allProjects ctx = 
    Database.getDocumentsByType documentType (makeModelFromJObject >> Some) ctx

