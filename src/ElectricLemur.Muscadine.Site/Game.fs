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
    let _id = FormFields.FormField.fromRequiredField ({
        Key = "_id"
        Label = "Id"
        getValueFromModel = (fun g -> g.Id)
        getValueFromContext = (fun ctx -> None)
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "_id" |> Option.get) })

    let _dateAdded = FormFields.FormField.fromRequiredField ({
        Key = "_dateAdded"
        Label = "Date Added"
        getValueFromModel = (fun g -> g.DateAdded)
        getValueFromContext = (fun ctx -> None)
        getValueFromJObject = (fun obj -> JObj.getter<System.DateTimeOffset> obj "_dateAdded" |> Option.get) })

    let name = FormFields.FormField.fromRequiredField ({
        Key = "name"
        Label = "Name"
        getValueFromModel = (fun g -> g.Name)
        getValueFromContext = (fun ctx -> HttpFormFields.fromContext ctx |> HttpFormFields.stringOptionValue "name")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "name" |> Option.get) })

    let description = FormFields.FormField.fromRequiredField ({
        Key = "description"
        Label = "Description"
        getValueFromModel = (fun g -> g.Description)
        getValueFromContext = (fun ctx -> HttpFormFields.fromContext ctx |> HttpFormFields.stringOptionValue "description")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "description" |> Option.get) })

    let slug = FormFields.FormField.fromRequiredField ({
        Key = "slug"
        Label = "Slug"
        getValueFromModel = (fun g -> g.Slug)
        getValueFromContext = (fun ctx -> HttpFormFields.fromContext ctx |> HttpFormFields.stringOptionValue "slug")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "slug" |> Option.get) })
    
    let coverImagePaths = FormFields.FormField.fromOptionalField ({
        Key = "coverImage"
        Label = "Cover Image"
        getValueFromModel = (fun g -> g.CoverImagePaths)
        getValueFromContext = (fun _ -> raise (new NotImplementedException("Cannot get coverImage from form fields")))
        getValueFromJObject = (fun obj -> JObj.getter<Image.ImagePaths> obj "coverImage"
        )})

let addEditView (g: Game option) allTags documentTags =

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
                    FormFields.View.makeTextRow Fields.name g
                    FormFields.View.makeTextAreaRow Fields.description 5 g
                    FormFields.View.makeTextRow Fields.slug g
                    FormFields.View.makeImageRow Fields.coverImagePaths g

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
    
    let o = ctx |> (FormFields.formGetter Fields.name) |> Option.get
    match requiredFieldsAreValid with
    | Ok _ -> 
        let getValue f = (f |> FormFields.formGetter) ctx |> Option.get
        let getNonFormFieldValue f = existing |> Option.bind (fun g -> (f |> FormFields.modelGetter) g)

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
    let getValue f = (f |> FormFields.jobjGetter) obj |> Option.get
    let getOptionalValue f = (f |> FormFields.jobjGetter) obj

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
    |> FormFields.setJObject g Fields._id
    |> FormFields.setJObject g Fields._dateAdded
    |> FormFields.setJObject g Fields.name
    |> FormFields.setJObject g Fields.description
    |> FormFields.setJObject g Fields.slug
    |> FormFields.setJObject g Fields.coverImagePaths
