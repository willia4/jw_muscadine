module Admin
open Giraffe
open Giraffe.ViewEngine
open Microsoft.AspNetCore.Http
open System.Security.Claims
open ElectricLemur.Muscadine.Site
open Newtonsoft.Json.Linq;
open Models;

module Views =
    let layout (pageTitle: string) (content: XmlNode list) = 
        html [] [
            head [] [
                title [] [ encodedText pageTitle ]
                link [ (_rel "stylesheet"); (_type "text/css"); (_href "/css/admin.scss") ]
                script [ _src "/js/admin.js" ] []
            ]
            body [] [
                div [ _class "site-title" ] [
                    encodedText "James Williams/"
                    a [ _href "/admin/" ] [encodedText "Admin"]
                ]
                div [ _class "body-content"] content
            ] 
        ]

    let makeIndexSection 
        (data: 'a seq) (header: string) (urlBase: string) (tableHeaders: string seq) 
        (makeTableRow: 'a -> XmlNode list) = 
        let safeHeader = header.ToLowerInvariant().Replace(" ", "-")

        div [ _class "section" ] [
            div [ _class "section-header" ] [
                span [] [ encodedText header ]
                a [ _class "add-new"; attr "data-for" safeHeader; _href $"{urlBase}/_new" ] [ encodedText "Add" ]
            ]

            table 
                [ _class "section-table" ] 
                ([
                    tr [] [ for h in tableHeaders -> (th [] [ encodedText h ])]
                ] |> Util.listPrepend [
                    for d in data ->
                        tr [] (makeTableRow d)
                ])

        ]

    let index (categories: Category seq) =
        [
            makeIndexSection 
                categories 
                "Categories" 
                "/admin/category" 
                [ "Short Name"; "Long Name"; "Description"; "" ]
                (fun c -> 
                    let makeUrl (c: Category) = $"/admin/category/{c.Id}"
                    [
                        td [] [ a [ _href (makeUrl c)] [ encodedText c.ShortName ]]
                        td [] [encodedText c.LongName ]
                        td [] [encodedText c.Description ]
                        td [] [
                            button [ _class "delete-button"
                                     attr "data-id" (c.Id.ToString()) 
                                     attr "data-name" c.ShortName 
                                     attr "data-url" $"/admin/category/{c.Id}" ]
                                   [ encodedText "Delete" ]
                        ]
                ])
        ] |> layout "Admin"

    let dataForm typeName (formKeys: (string * string) seq) (formData: Map<string, string> option) =
        let formVerb = if Option.isSome formData then "Edit" else "Add"
        let pageTitle = $"%s{formVerb} %s{typeName}"
        let formName = typeName.ToLowerInvariant().Replace(" ", "-")
        let formName = $"%s{formVerb.ToLowerInvariant()}-%s{formName}"

        let tableRow key caption autofocus =
            let value = 
                formData 
                |> Option.map (fun formData -> formData |> Map.tryFind key)
                |> Option.flatten
                |> Option.defaultValue ""

            tr [] [
                td [ _class "form-label" ] [
                    label [ _for key ] [ encodedText caption ]
                ]
                td [ _class "form-input" ] [
                    input (List.append [
                        _type "text"
                        _id key
                        _name key
                        _value value
                    ] (if autofocus then [ _autofocus ] else []))
                ]
            ]

        [
            div [ _class "page-title" ] [ encodedText $"%s{formVerb} %s{typeName}" ]
            form [ _name formName; _method "post" ] [
                table [] (List.append 
                        (formKeys 
                         |> Seq.indexed 
                         |> Seq.map (fun (i, (k, v)) -> tableRow k v (i = 0)) 
                         |> Seq.toList)
                
                    [
                        tr [] [
                                td [ _class "form-label "] []
                                td [ _class "form-input" ] [ input [ _type "submit"; _value "Save" ] ]
                            ]
                    ])
            ]
        ] |> layout pageTitle

let statusHandler: HttpHandler = 
    fun (next: HttpFunc) (ctx: HttpContext) ->
        let user = ctx.User
        let role = match user.FindFirst(ClaimTypes.Role) with
                   | null -> "No Role"
                   | claim -> claim.Value

        text role next ctx


let checkDatabaseHandler: HttpHandler =
    fun next ctx -> task {
        let! documentCount = Database.getDocumentCount ctx None
        let! categoryCount = Database.getDocumentCount ctx (Some "category")

        let result = $"Database results\n\nTotal documents: %d{documentCount}\nCategories: %d{categoryCount}"
        return! text result next ctx
    }



let addCrudGetHandler<'a> 
    (heading: string)
    (formKeysAndLabels: (string * string) seq): HttpHandler
    =
        htmlView (Views.dataForm heading formKeysAndLabels None)

let addCrudPostHandler<'a>
    (toJObject: 'a -> JObject) 
    (formKeysAndLabels: (string * string) seq)
    (fromFormData: Map<string, string> -> 'a option): HttpHandler =
        fun next ctx -> task {
            let data = 
                formKeysAndLabels
                |> Seq.map fst
                |> Util.getFormDataStrings ctx 
                |> Option.map fromFormData
                |> Option.flatten
                |> Option.map toJObject

            match data with
            | Some data -> 
                let! id = Database.insertDocument ctx data
                return! redirectTo false $"/admin/category/%s{id}" next ctx
            | None -> 
                return! (setStatusCode 400 >=> text "Invalid form data") next ctx
        }

let editCrudGetHandler<'a>
    (heading: string)
    (formKeysAndLabels: (string * string) seq) = 
        fun (id: string) next (ctx: HttpContext) -> task {
            let! doc = Database.getDocumentById id ctx
            let formData = 
                match doc with
                | None -> None
                | Some doc -> Util.getJObjectStrings doc (formKeysAndLabels |> Seq.map fst)
            return! htmlView (Views.dataForm heading formKeysAndLabels formData) next ctx
    } 
    
let editCrudPostHandler<'a>
    (toJObject: 'a -> JObject) 
    (formKeysAndLabels: (string * string) seq)
    (fromFormData: Map<string, string> -> 'a option): string -> HttpHandler =
        fun id next ctx -> task {
            let data = 
                formKeysAndLabels
                |> Seq.map fst
                |> Util.getFormDataStrings ctx 
                |> Option.map fromFormData
                |> Option.flatten
                |> Option.map toJObject

            match data with
            | Some data -> 
                data.["_id"] <- id
                do! Database.upsertDocument ctx data
                return! redirectTo false $"/admin/category/%s{id}" next ctx
            | None -> 
                return! (setStatusCode 400 >=> text "Invalid form data") next ctx
        }

let deleteCrudPostHandler: string -> HttpHandler =
    fun id next ctx -> task {
        do! Database.deleteDocument ctx id
        return! setStatusCode 200 next ctx
}

type CrudHandlers<'a> = {
    add_getHandler: HttpHandler;
    add_postHandler: HttpHandler;
    lister: HttpContext -> System.Threading.Tasks.Task<'a seq>
    edit_getHandler: string -> HttpHandler;
    edit_postHandler: string -> HttpHandler;
    delete_postHandler: string -> HttpHandler;
}

let getCrudHandlers<'a>
    (heading: string)
    (fromJObject: JObject -> 'a option) 
    (toJObject: 'a -> JObject) 
    (formKeysAndLabels: (string * string) seq)
    (fromFormData: Map<string, string> -> 'a option) = {
        add_getHandler = addCrudGetHandler heading formKeysAndLabels;
        add_postHandler = addCrudPostHandler toJObject formKeysAndLabels fromFormData;
        lister = Database.getDocumentsByType "category" fromJObject;
        edit_getHandler = editCrudGetHandler heading formKeysAndLabels;
        edit_postHandler = editCrudPostHandler toJObject formKeysAndLabels fromFormData;
        delete_postHandler = deleteCrudPostHandler;
    }

let categoryCrudHandlers: CrudHandlers<Category> = 
    getCrudHandlers
        "Category" 
        Category.FromJObject 
        Category.ToJObject 
        [
            (Category.Keys.shortName, "Short Name")
            (Category.Keys.longName, "Long Name")
            (Category.Keys.description, "Description")
        ]
        (fun formData -> 
            
            let values = Util.getMapStrings formData [ Category.Keys.shortName; Category.Keys.longName; Category.Keys.description ]
            match values with
            | None -> None
            | Some values -> Some {
                Id = System.Guid.NewGuid()
                DateAdded = System.DateTimeOffset.UtcNow
                ShortName = values.[Category.Keys.shortName]
                LongName = values.[Category.Keys.longName]
                Description = values.[Category.Keys.description]
            }
        )

let addCategoryGetHandler: HttpHandler = categoryCrudHandlers.add_getHandler
let addCategoryPostHandler: HttpHandler = categoryCrudHandlers.add_postHandler

let editCategoryGetHandler: string -> HttpHandler = categoryCrudHandlers.edit_getHandler
let editCategoryPostHandler: string -> HttpHandler = categoryCrudHandlers.edit_postHandler

let deleteCategoryPostHandler: string -> HttpHandler = categoryCrudHandlers.delete_postHandler

let indexHandler: HttpHandler = 
    fun next ctx -> task {
        let! categories = categoryCrudHandlers.lister ctx
        return! htmlView (Views.index categories) next ctx
    }