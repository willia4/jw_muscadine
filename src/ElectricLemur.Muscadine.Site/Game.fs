﻿module ElectricLemur.Muscadine.Site.Game
open Giraffe
open Giraffe.ViewEngine
open System
open Microsoft.AspNetCore.Http
open System.Threading.Tasks
open Newtonsoft.Json.Linq
open RequiredFields
open OptionalFields

let documentType = "game"

type Game = {
    Id: string;
    DateAdded: DateTimeOffset;
    Name: string;
    Description: string;
    Slug: string;
    Completed: bool;
    CoverImagePath: string option;
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

    let completed: RequiredFieldDescriptor<Game, bool> = {
        Key = "completed"
        Label = "Completed"
        getValueFromModel = (fun g -> g.Completed)
        getValueFromContext = (fun ctx -> FormFields.fromContext ctx |> FormFields.boolOptionValue "completed")
        getValueFromJObject = (fun obj -> JObj.getter<bool> obj "completed" |> Option.get) }
    
    let coverImagePath: OptionalFieldDescriptor<Game, string> = {
        Key = "coverImage"
        Label = "Cover Image"
        getValueFromModel = (fun g -> g.CoverImagePath)
        getValueFromContext = (fun _ -> raise (new NotImplementedException("Cannot get coverImage from form fields")))
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "coverImage")
    }

let addEditView (g: Game option) =

    let pageTitle = match g with
                    | None -> "Add Game" 
                    | Some g-> $"Edit Game %s{g.Name}"

    let makeTextRow ff = 
        let v = g |> Option.map (RequiredFields.modelGetter ff) 
        Items.makeTextInputRow (RequiredFields.label ff) (RequiredFields.key ff) v

    let makecheckboxRow ff = 
        let v = g |> Option.map (RequiredFields.modelGetter ff)
        Items.makeCheckboxInputRow (RequiredFields.label ff) (RequiredFields.key ff) v

    let makeImageRow ff =
        let v = g |> Option.map (OptionalFields.modelGetter ff) |> Option.flatten
        Items.makeImageInputRow (OptionalFields.label ff) (OptionalFields.key ff) v

    Items.layout pageTitle [
        div [ _class "page-title" ] [ encodedText pageTitle ]
        form [ _name "game-form"; _method "post"; _enctype "multipart/form-data" ] [
                table [] [
                    makeTextRow Fields.name
                    makeTextRow Fields.description  
                    makeTextRow Fields.slug
                    makeImageRow Fields.coverImagePath
                    makecheckboxRow Fields.completed
                    tr [] [
                        td [] []
                        td [] [ input [ _type "submit"; _value "Save" ] ]
                    ]
                ]
            ]
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
        let getOptionalValue f = existing |> Option.map (fun g -> (f |> OptionalFields.modelGetter) g) |> Option.flatten

        let g = {
            Id =            id
            DateAdded =     dateAdded
            Name =          Fields.name |> getValue
            Description =   Fields.description |> getValue
            Slug =          Fields.slug |> getValue
            Completed =     Fields.completed |> getValue
            CoverImagePath = Fields.coverImagePath |> getOptionalValue
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
        Completed =     Fields.completed |> getValue
        CoverImagePath = Fields.coverImagePath |> getOptionalValue
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
    |> RequiredFields.setJObject g Fields.completed
    |> OptionalFields.setJObject g Fields.coverImagePath


let addHandler_get =
    fun next (ctx: HttpContext) ->
        htmlView (addEditView None) next ctx

let addHandler_post : HttpHandler = 
    fun next (ctx: HttpContext) -> task {
        let! g = makeAndValidateModelFromContext None ctx
        
        match g with
        | Ok g ->
            let data = makeJObjectFromModel g
            let! id = Database.insertDocument ctx data
            return! (redirectTo false $"/admin/game/%s{id}") next ctx
        | Error msg -> return! (setStatusCode 400 >=> text msg) next ctx
    }

let editHandler_get id =
    fun next (ctx: HttpContext) -> task {
        let! existing = Database.getDocumentById id ctx
        let existing = existing |> Option.map makeModelFromJObject

        return! htmlView (addEditView existing) next ctx
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
                    Items.handleFileUpload ctx documentType g.Id Fields.coverImagePath.Key g.CoverImagePath (fun newPath -> { g with CoverImagePath = newPath })

                match coverImageUploadResult with
                | Error msg -> return! (setStatusCode 400 >=> text msg) next ctx
                | Ok g -> 
                    let data = makeJObjectFromModel g
                    do! Database.upsertDocument ctx data
                    return! (redirectTo false $"/admin/game/%s{id}") next ctx
            | Error msg -> return! (setStatusCode 400 >=> text msg) next ctx
    }

let deleteHandler_delete id =
    fun next ctx -> task {
        do! Database.deleteDocument ctx id
        return! setStatusCode 200 next ctx
    }
    
let allGames ctx = 
    Database.getDocumentsByType documentType (makeModelFromJObject >> Some) ctx

