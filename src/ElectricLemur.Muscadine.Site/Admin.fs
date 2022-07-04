module Admin
open Giraffe
open Giraffe.ViewEngine
open Microsoft.AspNetCore.Http
open System.Security.Claims
open ElectricLemur.Muscadine.Site
open Newtonsoft.Json.Linq;
open Game;
open Book;
open System.Threading.Tasks

module Views =
    let layout (pageTitle: string) (content: XmlNode list) = 
        html [] [
            head [] [
                title [] [ encodedText pageTitle ]
                link [ (_rel "stylesheet"); (_type "text/css"); (_href "/css/admin.css") ]
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

    let index (games: seq<Game>) (books: seq<Book>) =
        [
            makeIndexSection 
                games 
                "Games" 
                "/admin/game" 
                [ "Short Name"; "Description"; "Slug"; "" ]
                (fun g -> 
                    let makeUrl (g: Game) = $"/admin/game/{g.Id}"
                    [
                        td [] [ a [ _href (makeUrl g)] [ encodedText g.Name ]]
                        td [] [ encodedText g.Description ]
                        td [] [ encodedText g.Slug ]
                        td [] [
                            button [ _class "delete-button"
                                     attr "data-id" (string g.Id) 
                                     attr "data-name" g.Name 
                                     attr "data-url" $"/admin/game/{g.Id}" ]
                                   [ encodedText "Delete" ]
                        ]
                ])

            makeIndexSection
                books
                "Books"
                "/admin/book"
                [ "Title"; "Description"; "Slug"; "" ]
                (fun b -> 
                    let makeUrl (b: Book) = $"/admin/book/{b.Id}"
                    [
                        td [] [ a [ _href (makeUrl b)] [ encodedText b.Title ]]
                        td [] [ encodedText b.Description ]
                        td [] [ encodedText b.Slug ]
                        td [] [
                            button [ _class "delete-button"
                                     attr "data-id" (string b.Id) 
                                     attr "data-name" b.Title 
                                     attr "data-url" $"/admin/book/{b.Id}" ]
                                   [ encodedText "Delete" ]
                        ]
                ])
                
        ] |> layout "Admin"

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
        let! gameCount = Database.getDocumentCount ctx (Some Game.documentType)

        let result = $"Database results\n\nTotal documents: %d{documentCount}\Games: %d{gameCount}"
        return! text result next ctx
    }

let indexHandler: HttpHandler = 
    fun next ctx -> task {
        let gameTask = Game.allGames ctx
        let bookTask = Book.allBooks ctx

        let tasks = [| 
            (gameTask :> System.Threading.Tasks.Task)
            (bookTask :> System.Threading.Tasks.Task)
        |]

        let all = System.Threading.Tasks.Task.WhenAll(tasks)
        try
            do! all
        with
        | ex -> ()

        if (all.Status = TaskStatus.RanToCompletion) then
            let games = gameTask.Result
            let books = bookTask.Result

            return! htmlView (Views.index games books) next ctx
        else
            return! (setStatusCode 500 >> text "Could not load documents") next ctx
        //let! games = Game.allGames ctx
        //return! htmlView (Views.index games) next ctx
    }