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
        getValueFromContext = (fun ctx -> HttpFormFields.fromContext ctx |> HttpFormFields.stringOptionValue "name")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "name" |> Option.get) }

    let description: RequiredFieldDescriptor<Game, string> = {
        Key = "description"
        Label = "Description"
        getValueFromModel = (fun g -> g.Description)
        getValueFromContext = (fun ctx -> HttpFormFields.fromContext ctx |> HttpFormFields.stringOptionValue "description")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "description" |> Option.get) }

    let slug: RequiredFieldDescriptor<Game, string> = {
        Key = "slug"
        Label = "Slug"
        getValueFromModel = (fun g -> g.Slug)
        getValueFromContext = (fun ctx -> HttpFormFields.fromContext ctx |> HttpFormFields.stringOptionValue "slug")
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

    let makeTextAreaRow ff lines =
        let v = g |> Option.map (RequiredFields.modelGetter ff)
        Items.makeTextAreaInputRow (RequiredFields.label ff) (RequiredFields.key ff) lines v

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
                    makeTextAreaRow Fields.description 5
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

let validateModel (id: string) (g: Game) ctx =
    let stringFieldUniquenessValidator field = RequiredFields.stringFieldUniquenessValidator ctx documentType id field

    Ok g
    |> Task.fromResult
    |> Task.map (Items.performValidation (RequiredFields.requiredStringValidator Fields.name))
    |> Task.map (Items.performValidation (RequiredFields.requiredStringValidator Fields.description))
    |> Task.map (Items.performValidation (RequiredFields.requiredStringValidator Fields.slug))
    |> Task.bind (Items.performValidationAsync (stringFieldUniquenessValidator Fields.name))
    |> Task.bind (Items.performValidationAsync (stringFieldUniquenessValidator Fields.slug))


let makeAndValidateModelFromContext (existing: Game option) (ctx: HttpContext): Task<Result<Game, string>> =
    let id = existing |> Option.map (fun g -> g.Id) |> Option.defaultValue (string (Util.newGuid ()))
    let dateAdded = existing |> Option.map (fun g -> g.DateAdded) |> Option.defaultValue System.DateTimeOffset.UtcNow

    let fields = HttpFormFields.fromContext ctx
    let requiredFieldsAreValid = 
        Ok ()
        |> HttpFormFields.checkRequiredStringField fields (RequiredFields.key Fields.name)
        |> HttpFormFields.checkRequiredStringField fields (RequiredFields.key Fields.description)
        |> HttpFormFields.checkRequiredStringField fields (RequiredFields.key Fields.slug)
    
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
        validateModel id g ctx
    | Error msg -> Task.fromResult (Error msg)


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
    |> RequiredFields.setJObject g Fields._id
    |> RequiredFields.setJObject g Fields._dateAdded
    |> RequiredFields.setJObject g Fields.name
    |> RequiredFields.setJObject g Fields.description
    |> RequiredFields.setJObject g Fields.slug
    |> OptionalFields.setJObject g Fields.coverImagePaths
