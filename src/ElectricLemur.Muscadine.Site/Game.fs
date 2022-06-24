﻿module ElectricLemur.Muscadine.Site.Game
open Giraffe
open Giraffe.ViewEngine
open System
open Microsoft.AspNetCore.Http

let documentType = "game"

type Game = {
    Id: string;
    DateAdded: DateTimeOffset;
    Name: string;
    Description: string;
    Slug: string;
}

let addEditView (g: Game option) =
    let modelStringValue fieldName = 
        match g with
        | Some g -> Items.Helpers.modelStringValue g fieldName
        | None -> None

    let makeRow label formEl = 
        tr [] [
            td [ _class "form-label" ] [ encodedText label ]
            td [ _class "form-input" ] [ formEl ]
        ]

    let makeTextRow label key =
        let v = modelStringValue key
        let el = input [ 
            _type "text"
            _id key
            _name key
            _value (v |> Option.defaultValue "")
        ]
        makeRow label el

    let pageTitle = match g with
                    | None -> "Add Game" 
                    | Some g-> $"Edit Game %s{g.Name}"

    Items.layout pageTitle [
        div [ _class "page-title" ] [ encodedText pageTitle ]
        form [ _name "game-form"; _method "post" ] [
                table [] [
                    makeTextRow "Name*" "Name"
                    makeTextRow "Description*" "Description"
                    makeTextRow "Slug*" "Slug"
                    tr [] [
                        td [] []
                        td [] [ input [ _type "submit"; _value "Save" ] ]
                    ]
                ]
            ]
    ]

let validate (id: string) (g: Game) ctx = task {
    let uniquenessChecker = fun fieldName g -> task {
        let value = Items.Helpers.modelStringValue g fieldName 
        match value with
        | None -> return Ok g
        | Some s -> 
            let! valid = Database.checkUniqueness documentType fieldName s id ctx
            match valid with
            | true -> return Ok g
            | false -> return Error $"%s{fieldName} must be unique"
    }
    
    let valid = Ok g |> Items.Helpers.validateRequiredFields
    let! valid = valid |> Items.Helpers.performValidation (uniquenessChecker "Name")
    let! valid = valid |> Items.Helpers.performValidation (uniquenessChecker "Slug")

    return valid
}

let addHandler_get =
    fun next (ctx: HttpContext) ->
        htmlView (addEditView None) next ctx

let addHandler_post : HttpHandler = 
    fun next (ctx: HttpContext) -> task {
        let id = string (Util.newGuid ())
        let dateAdded = System.DateTimeOffset.UtcNow

        match Items.Helpers.makeModelFromFormFields<Game> id dateAdded (ctx.Request.Form) with
        | Ok m -> 
            let! validated = validate id m ctx
            match validated with
            | Ok m -> 
                let data = Items.Helpers.makeJObjectFromModel m documentType
                let! id = Database.insertDocument ctx data
                return! (redirectTo false $"/admin/game/%s{id}") next ctx
            | Error msg -> 
                return! (setStatusCode 400 >=> text msg) next ctx
        | Error msg -> 
            return! (setStatusCode 400 >=> text msg) next ctx
    }

let editHandler_get id =
    fun next (ctx: HttpContext) -> task {
        let! existing = Database.getDocumentById id ctx
        let existing = existing |> Option.map Items.Helpers.makeModelFromJObject<Game>

        return! htmlView (addEditView existing) next ctx
    }

let editHandler_post id : HttpHandler =
    fun next (ctx: HttpContext) -> task {
        let! existing = Database.getDocumentById id ctx
        match existing with
        | None -> return! (setStatusCode 404) next ctx
        | Some existing ->
            let existing = Items.Helpers.makeModelFromJObject<Game> existing
            match Items.Helpers.makeModelFromFormFields<Game> id existing.DateAdded ctx.Request.Form with
            | Ok m ->
                let! validated = validate id m ctx
                match validated with
                | Ok m ->
                    let data = Items.Helpers.makeJObjectFromModel m documentType
                    do! Database.upsertDocument ctx data
                    return! (redirectTo false $"/admin/game/%s{id}") next ctx
                | Error msg ->
                    return! (setStatusCode 400 >=> text msg) next ctx
            | Error msg ->
                return! (setStatusCode 400 >=> text msg) next ctx
    }

let deleteHandler_delete id =
    fun next ctx -> task {
        do! Database.deleteDocument ctx id
        return! setStatusCode 200 next ctx
    }
    
let allGames ctx = 
    Database.getDocumentsByType documentType (Items.Helpers.makeModelFromJObject<Game> >> Some) ctx
