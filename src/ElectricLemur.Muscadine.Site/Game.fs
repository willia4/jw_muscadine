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
    let _id: RequiredFieldDescriptor<Game, string> = {
        Key = "_id"
        Label = "Id"
        getValueFromModel = (fun g -> g.Id)
        getValueFromContext = (fun ctx -> None)
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "_id" |> Option.get) }

    let _dateAdded: RequiredFieldDescriptor<Game, System.DateTimeOffset> = {
        Key = "_dateAdded"
        Label = "Date Added"
        getValueFromModel = (fun g -> g.DateAdded)
        getValueFromContext = (fun ctx -> None)
        getValueFromJObject = (fun obj -> JObj.getter<System.DateTimeOffset> obj "_dateAdded" |> Option.get) }

    let name: RequiredFieldDescriptor<Game, string> = {
        Key = "name"
        Label = "Name"
        getValueFromModel = (fun g -> g.Name)
        getValueFromContext = (fun ctx -> FormFields.fromContext ctx |> FormFields.stringOptionValue "name")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "name" |> Option.get) }

    let description: RequiredFieldDescriptor<Game, string> = {
        Key = "description"
        Label = "Description"
        getValueFromModel = (fun g -> g.Description)
        getValueFromContext = (fun ctx -> FormFields.fromContext ctx |> FormFields.stringOptionValue "description")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "description" |> Option.get) }

    let slug: RequiredFieldDescriptor<Game, string> = {
        Key = "slug"
        Label = "Slug"
        getValueFromModel = (fun g -> g.Slug)
        getValueFromContext = (fun ctx -> FormFields.fromContext ctx |> FormFields.stringOptionValue "slug")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "slug" |> Option.get) }
    
    let coverImagePaths: OptionalFieldDescriptor<Game, Image.ImagePaths> = {
        Key = "coverImage"
        Label = "Cover Image"
        getValueFromModel = (fun g -> g.CoverImagePaths)
        getValueFromContext = (fun _ -> raise (new NotImplementedException("Cannot get coverImage from form fields")))
        getValueFromJObject = (fun obj -> JObj.getter<Image.ImagePaths> obj "coverImage"
        )
    }

let addEditView (g: Game option) allTags documentTags =

    let pageTitle = match g with
                    | None -> "Add Game" 
                    | Some g-> $"Edit Game %s{g.Name}"

    let pageData =
        match g with
        | Some g -> Map.ofList [ ("id", Items.pageDataType.String g.Id); ("slug", Items.pageDataType.String  "game")]
        | None -> Map.empty

    let makeTextRow ff =
        let v = g |> Option.map (RequiredFields.modelGetter ff) 
        Items.makeTextInputRow (RequiredFields.label ff) (RequiredFields.key ff) v

    let makecheckboxRow ff = 
        let v = g |> Option.map (RequiredFields.modelGetter ff)
        Items.makeCheckboxInputRow (RequiredFields.label ff) (RequiredFields.key ff) v

    let makeImageRow (ff: OptionalFields.OptionalFieldDescriptor<Game, Image.ImagePaths>) =
        let v = g 
                |> Option.map (OptionalFields.modelGetter ff) 
                |> Option.flatten
                |> Option.map (fun paths -> paths.Size512)

        Items.makeImageInputRow (OptionalFields.label ff) (OptionalFields.key ff) v

    Items.layout pageTitle pageData [
        div [ _class "page-title" ] [ encodedText pageTitle ]
        form [ _name "game-form"; _method "post"; _enctype "multipart/form-data" ] [
                table [] [
                    makeTextRow Fields.name
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

        (match g with
        | None -> Util.emptyDiv
        | Some _ ->
            div [ _class "microblog-container section" ] [])
    ]

let validateModel (id: string) (g: Game) ctx = task {
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

let makeAndValidateModelFromContext (existing: Game option) (ctx: HttpContext): Task<Result<Game, string>> = task {
    let id = existing |> Option.map (fun g -> g.Id) |> Option.defaultValue (string (Util.newGuid ()))
    let dateAdded = existing |> Option.map (fun g -> g.DateAdded) |> Option.defaultValue System.DateTimeOffset.UtcNow

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
        let getNonFormFieldValue f = existing |> Option.map (fun g -> (f |> OptionalFields.modelGetter) g) |> Option.flatten

        let g = {
            Id =            id
            DateAdded =     dateAdded
            Name =          Fields.name |> getValue
            Description =   Fields.description |> getValue
            Slug =          Fields.slug |> getValue
            CoverImagePaths = Fields.coverImagePaths |> getNonFormFieldValue
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
        Name =          Fields.name |> getValue
        Description =   Fields.description |> getValue
        Slug =          Fields.slug |> getValue
        CoverImagePaths = Fields.coverImagePaths |> getOptionalValue
    }

let makeJObjectFromModel (g: Game) =
    (new JObject())
    |> (fun obj -> 
            obj.["_documentType"] <- documentType
            obj)
    |> RequiredFields.setJObject g Fields._id
    |> RequiredFields.setJObject g Fields._dateAdded
    |> RequiredFields.setJObject g Fields.name
    |> RequiredFields.setJObject g Fields.description
    |> RequiredFields.setJObject g Fields.slug
    |> OptionalFields.setJObject g Fields.coverImagePaths


let addHandler_get =
    fun next (ctx: HttpContext) -> task {
        let! allTags = Tag.getExistingTags ctx

        return! htmlView (addEditView None allTags []) next ctx
    }


let addHandler_post : HttpHandler = 
    fun next (ctx: HttpContext) -> task {
        let! g = makeAndValidateModelFromContext None ctx
        
        match g with
        | Ok g ->
            let! coverImageUploadResult =
                Items.handleImageUpload ctx documentType g.Id Fields.coverImagePaths.Key g.CoverImagePaths (fun newPaths -> { g with CoverImagePaths = newPaths})

            match coverImageUploadResult with
            | Error msg -> return! (setStatusCode 400 >=> text msg) next ctx
            | Ok g ->
                let data = makeJObjectFromModel g
                let! id = Database.insertDocument ctx data
                do! Tag.saveTagsForForm documentType id Tag.formKey ctx

                return! (redirectTo false $"/admin/game/%s{id}") next ctx
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
            let! g = makeAndValidateModelFromContext (Some existing) ctx
            match g with
            | Ok g ->
                let! coverImageUploadResult = 
                    Items.handleImageUpload ctx documentType g.Id Fields.coverImagePaths.Key g.CoverImagePaths (fun newPaths -> { g with CoverImagePaths = newPaths})

                match coverImageUploadResult with
                | Error msg -> return! (setStatusCode 400 >=> text msg) next ctx
                | Ok g -> 
                    let data = makeJObjectFromModel g
                    do! Database.upsertDocument ctx data
                    do! Tag.saveTagsForForm documentType g.Id Tag.formKey ctx

                    return! (redirectTo false $"/admin/game/%s{id}") next ctx
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
    
let allGames ctx =
    Database.getDocumentsByType documentType (makeModelFromJObject >> Some) Database.NoLimit ctx

