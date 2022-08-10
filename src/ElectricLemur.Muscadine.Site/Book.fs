module ElectricLemur.Muscadine.Site.Book
open Giraffe
open Giraffe.ViewEngine
open System
open Microsoft.AspNetCore.Http
open System.Threading.Tasks
open Newtonsoft.Json.Linq
open RequiredFields
open OptionalFields

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
    let _id: RequiredFieldDescriptor<Book, string> = {
        Key = "_id"
        Label = "Id"
        getValueFromModel = (fun b -> b.Id)
        getValueFromContext = (fun ctx -> None)
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "_id" |> Option.get) }

    let _dateAdded: RequiredFieldDescriptor<Book, System.DateTimeOffset> = {
        Key = "_dateAdded"
        Label = "Date Added"
        getValueFromModel = (fun b -> b.DateAdded)
        getValueFromContext = (fun ctx -> None)
        getValueFromJObject = (fun obj -> JObj.getter<System.DateTimeOffset> obj "_dateAdded" |> Option.get) }

    let title: RequiredFieldDescriptor<Book, string> = {
        Key = "name"
        Label = "Name"
        getValueFromModel = (fun b -> b.Title)
        getValueFromContext = (fun ctx -> FormFields.fromContext ctx |> FormFields.stringOptionValue "name")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "name" |> Option.get) }

    let description: RequiredFieldDescriptor<Book, string> = {
        Key = "description"
        Label = "Description"
        getValueFromModel = (fun b -> b.Description)
        getValueFromContext = (fun ctx -> FormFields.fromContext ctx |> FormFields.stringOptionValue "description")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "description" |> Option.get) }

    let slug: RequiredFieldDescriptor<Book, string> = {
        Key = "slug"
        Label = "Slug"
        getValueFromModel = (fun b -> b.Slug)
        getValueFromContext = (fun ctx -> FormFields.fromContext ctx |> FormFields.stringOptionValue "slug")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "slug" |> Option.get) }
    
    let coverImagePaths: OptionalFieldDescriptor<Book, Image.ImagePaths> = {
        Key = "coverImage"
        Label = "Cover Image"
        getValueFromModel = (fun b -> b.CoverImagePaths)
        getValueFromContext = (fun _ -> raise (new NotImplementedException("Cannot get coverImage from form fields")))
        getValueFromJObject = (fun obj -> JObj.getter<Image.ImagePaths> obj "coverImage"
        )
    }

let addEditView (b: Book option) allTags documentTags =

    let pageTitle = match b with
                    | None -> "Add Book" 
                    | Some b-> $"Edit Book %s{b.Title}"

    let pageData =
        match b with
        | Some b -> Map.ofList [ ("id", Items.pageDataType.String b.Id); ("slug", Items.pageDataType.String  "book")]
        | None -> Map.empty

    let makeTextRow ff =
        let v = b |> Option.map (RequiredFields.modelGetter ff) 
        Items.makeTextInputRow (RequiredFields.label ff) (RequiredFields.key ff) v

    let makecheckboxRow ff = 
        let v = b |> Option.map (RequiredFields.modelGetter ff)
        Items.makeCheckboxInputRow (RequiredFields.label ff) (RequiredFields.key ff) v

    let makeImageRow (ff: OptionalFields.OptionalFieldDescriptor<Book, Image.ImagePaths>) =
        let v = b
                |> Option.map (OptionalFields.modelGetter ff) 
                |> Option.flatten
                |> Option.map (fun paths -> paths.Size512)

        Items.makeImageInputRow (OptionalFields.label ff) (OptionalFields.key ff) v

    Items.layout pageTitle pageData [
        div [ _class "page-title" ] [ encodedText pageTitle ]
        form [ _name "book-form"; _method "post"; _enctype "multipart/form-data" ] [
                table [] [
                    makeTextRow Fields.title
                    makeTextRow Fields.description  
                    makeTextRow Fields.slug
                    makeImageRow Fields.coverImagePaths

                    Items.makeTagsInputRow "Tags" Tag.formKey allTags documentTags

                    tr [] [
                        td [] []
                        td [] [ input [ _type "submit"; _value "Save" ] ]
                    ]
                ]
            ]

        (match b with
        | None -> Util.emptyDiv
        | Some _ ->
            div [ _class "microblog-container section" ] [])
    ]

let validateModel (id: string) (g: Book) ctx =
    let stringFieldUniquenessValidator field = RequiredFields.stringFieldUniquenessValidator ctx documentType id field

    Ok g
    |> Task.fromResult
    |> Task.map (Items.performValidation (RequiredFields.requiredStringValidator Fields.title))
    |> Task.map (Items.performValidation (RequiredFields.requiredStringValidator Fields.description))
    |> Task.map (Items.performValidation (RequiredFields.requiredStringValidator Fields.slug))
    |> Task.bind (Items.performValidationAsync (stringFieldUniquenessValidator Fields.title))
    |> Task.bind (Items.performValidationAsync (stringFieldUniquenessValidator Fields.slug))

let makeAndValidateModelFromContext (existing: Book option) (ctx: HttpContext): Task<Result<Book, string>> =
    let id = existing |> Option.map (fun g -> g.Id) |> Option.defaultValue (string (Util.newGuid ()))
    let dateAdded = existing |> Option.map (fun g -> g.DateAdded) |> Option.defaultValue System.DateTimeOffset.UtcNow

    let fields = FormFields.fromContext ctx
    let requiredFieldsAreValid = 
        Ok ()
        |> FormFields.checkRequiredStringField fields (RequiredFields.key Fields.title)
        |> FormFields.checkRequiredStringField fields (RequiredFields.key Fields.description)
        |> FormFields.checkRequiredStringField fields (RequiredFields.key Fields.slug)
    
    let o = ctx |> (RequiredFields.formGetter Fields.title) |> Option.get
    match requiredFieldsAreValid with
    | Ok _ -> 
        let getValue f = (f |> RequiredFields.formGetter) ctx |> Option.get
        let getNonFormFieldValue f = existing |> Option.map (fun g -> (f |> OptionalFields.modelGetter) g) |> Option.flatten

        let g = {
            Id =            id
            DateAdded =     dateAdded
            Title =         Fields.title |> getValue
            Description =   Fields.description |> getValue
            Slug =          Fields.slug |> getValue
            CoverImagePaths = Fields.coverImagePaths |> getNonFormFieldValue
        }
        validateModel id g ctx
    | Error msg -> Task.fromResult (Error msg)


let makeModelFromJObject (obj: JObject) =
    let getValue f = (f |> RequiredFields.jobjGetter) obj
    let getOptionalValue f = (f |> OptionalFields.jobjGetter) obj

    {
        Id =            Fields._id |> getValue
        DateAdded =     Fields._dateAdded |> getValue
        Title =         Fields.title|> getValue
        Description =   Fields.description |> getValue
        Slug =          Fields.slug |> getValue
        CoverImagePaths = Fields.coverImagePaths |> getOptionalValue
    }

let makeJObjectFromModel (g: Book) =
    (new JObject())
    |> (fun obj -> 
            obj.["_documentType"] <- documentType
            obj)
    |> RequiredFields.setJObject g Fields._id
    |> RequiredFields.setJObject g Fields._dateAdded
    |> RequiredFields.setJObject g Fields.title
    |> RequiredFields.setJObject g Fields.description
    |> RequiredFields.setJObject g Fields.slug
    |> OptionalFields.setJObject g Fields.coverImagePaths


let allBooks ctx =
    Database.getDocumentsByType documentType (makeModelFromJObject >> Some) Database.NoLimit ctx

module Handlers =
    let addHandler_get =
        fun next (ctx: HttpContext) -> task {
            let! allTags = Tag.getExistingTags ctx
            return! htmlView (addEditView None allTags []) next ctx
        }
            

    let addHandler_post : HttpHandler = 
        fun next (ctx: HttpContext) -> task {
            let! b = makeAndValidateModelFromContext None ctx

            match b with
            | Ok b ->
                let! coverImageUploadResult =
                    Items.handleImageUpload ctx documentType b.Id Fields.coverImagePaths.Key b.CoverImagePaths (fun newPaths -> { b with CoverImagePaths = newPaths})

                match coverImageUploadResult with
                | Error msg -> return! (setStatusCode 400 >=> text msg) next ctx
                | Ok g ->
                    let data = makeJObjectFromModel g
                    let! id = Database.insertDocument ctx data
                    do! Tag.saveTagsForForm documentType id Tag.formKey ctx

                    return! (redirectTo false $"/admin/book/%s{id}") next ctx
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
                let! b = makeAndValidateModelFromContext (Some existing) ctx
                match b with
                | Ok b ->
                    let! coverImageUploadResult = 
                        Items.handleImageUpload ctx documentType b.Id Fields.coverImagePaths.Key b.CoverImagePaths (fun newPaths -> { b with CoverImagePaths = newPaths})

                    match coverImageUploadResult with
                    | Error msg -> return! (setStatusCode 400 >=> text msg) next ctx
                    | Ok b -> 
                        let data = makeJObjectFromModel b
                        do! Database.upsertDocument ctx data
                        do! Tag.saveTagsForForm documentType b.Id Tag.formKey ctx

                        return! (redirectTo false $"/admin/book/%s{id}") next ctx
                | Error msg -> return! (setStatusCode 400 >=> text msg) next ctx
        }

    let deleteHandler_delete id =
        fun next ctx -> task {
            let! existing = Database.getDocumentById id ctx
            let existing = existing |> Option.map makeModelFromJObject
            let existingCoverImage = existing |> Option.map (fun e -> e.CoverImagePaths) |> Option.flatten

            match existingCoverImage with
            | Some existingCoverImage -> Image.deleteAllImages existingCoverImage ctx
            | None -> ()

            do! Tag.clearTagsForDocument documentType id ctx
            do! Microblog.deleteAllMicroblogsFromItem documentType id ctx
            do! Database.deleteDocument ctx id
            return! setStatusCode 200 next ctx
        }

