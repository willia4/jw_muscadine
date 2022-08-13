module Admin
open Giraffe
open Giraffe.ViewEngine
open Microsoft.AspNetCore.Http
open System.Security.Claims
open ElectricLemur.Muscadine.Site
open Newtonsoft.Json.Linq;
open Game;
open Book;
open Project;
open System.Threading.Tasks

module Views =
    let layout (pageTitle: string) (content: XmlNode list) ctx =
        html [] [
            head [] [
                title [] [ encodedText pageTitle ]
                (Util.cssLinkTag "admin.scss" ctx)
                (Util.javascriptTag "admin.js" ctx)
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
                ] |> List.prepend [
                    for d in data ->
                        tr [] (makeTableRow d)
                ])

        ]

    let index (games: seq<Game * List<string>> ) (books: seq<Book * List<string>>) (projects: seq<Project * List<string>>) (ctx: HttpContext) =
        let makeTagsCell tags = 
            div [ _class "tags" ] (tags |> List.sort |> List.map (fun t -> div [ _class "tag" ] [encodedText t]))
        let content = [
            makeIndexSection 
                games 
                "Games" 
                "/admin/game" 
                [ "Cover"; "Short Name"; "Description"; "Slug"; "Tags"; "" ]
                (fun g -> 
                    let (g, tags) = g
                    let makeUrl (g: Game) = $"/admin/game/{g.Id}"
                    [
                        td [ _class "icon-cell" ] (
                            let path = g.CoverImagePaths |> Option.map (fun p -> p.Size64) |> Util.addRootPath "/images"
                            match path with
                               | Some path -> [ img [ _src path ]]
                               | None -> [])
                        td [] [ a [ _href (makeUrl g)] [ encodedText g.Name ]]
                        td [] [ encodedText g.Description ]
                        td [] [ encodedText g.Slug ]
                        td [ _class "tag-cell" ] [ makeTagsCell tags ]
                        td [ _class "delete-cell" ] [
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
                [ "Cover"; "Title"; "Description"; "Slug"; "Tags"; "" ]
                (fun b -> 
                    let (b, tags) = b
                    let makeUrl (b: Book) = $"/admin/book/{b.Id}"
                    [
                        td [ _class "icon-cell" ] (
                            let path = b.CoverImagePaths |> Option.map (fun p -> p.Size64) |> Util.addRootPath "/images"
                            match path with
                               | Some path -> [ img [ _src path ]]
                               | None -> [])
                        td [] [ a [ _href (makeUrl b)] [ encodedText b.Title ]]
                        td [] [ encodedText b.Description ]
                        td [] [ encodedText b.Slug ]
                        td [ _class "tag-cell" ] [ makeTagsCell tags ]
                        td [ _class "delete-cell" ] [
                            button [ _class "delete-button"
                                     attr "data-id" (string b.Id) 
                                     attr "data-name" b.Title 
                                     attr "data-url" $"/admin/book/{b.Id}" ]
                                   [ encodedText "Delete" ]
                        ]
                ])
                
            makeIndexSection
                projects
                "Projects"
                "/admin/project"
                [ "Icon"; "Name"; "Description"; "Slug"; "Tags"; "" ]
                (fun p -> 
                    let (p, tags) = p
                    let makeUrl (p: Project) = $"/admin/project/{p.Id}"
                    [
                        td [ _class "icon-cell" ] (
                            let path = p.IconImagePaths |> Option.map (fun p -> p.Size64) |> Util.addRootPath "/images"
                            match path with
                               | Some path -> [ img [ _src path ]]
                               | None -> [])
                        td [] [ a [ _href (makeUrl p)] [ encodedText p.Name ]]
                        td [] [ encodedText p.Description ]
                        td [] [ encodedText p.Slug ]
                        td [ _class "tag-cell" ] [ makeTagsCell tags ]
                        td [ _class "delete-cell" ] [
                            button [ _class "delete-button"
                                     attr "data-id" (string p.Id) 
                                     attr "data-name" p.Name 
                                     attr "data-url" $"/admin/book/{p.Id}" ]
                                   [ encodedText "Delete" ]
                        ]
                ])
        ]

        layout "Admin" content ctx

module Handlers =
    let GET_status: HttpHandler =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            let user = ctx.User
            let role = match user.FindFirst(ClaimTypes.Role) with
                       | null -> "No Role"
                       | claim -> claim.Value

            text role next ctx


    let GET_checkDatabase: HttpHandler =
        fun next ctx -> task {
            let! documentCount = Database.getDocumentCount ctx None
            let! gameCount = Database.getDocumentCount ctx (Some Game.documentType)

            let result = $"Database results\n\nTotal documents: %d{documentCount}\Games: %d{gameCount}"
            return! text result next ctx
        }

    let GET_index: HttpHandler =
        fun next ctx -> task {
            let! games = Game.allGames ctx
            let! books = Book.allBooks ctx
            let! projects = Project.allProjects ctx

            let! gameTags = Tag.loadTagsForDocuments Game.documentType (games |> Seq.map (fun x -> x.Id)) ctx
            let! bookTags = Tag.loadTagsForDocuments Book.documentType (books |> Seq.map (fun x -> x.Id)) ctx
            let! projectTags = Tag.loadTagsForDocuments Project.documentType (projects |> Seq.map (fun x -> x.Id)) ctx

            let matchItemsToTags (items: seq<'a>) (idGetter: 'a -> string) tags = 
                items
                |> Seq.map (fun i -> 
                    let id = idGetter i
                    let tags = tags |> Map.tryFind id |> Option.defaultValue []
                    (i, tags)
                )

            let games = matchItemsToTags games (fun x -> x.Id) gameTags
            let books = matchItemsToTags books (fun x -> x.Id) bookTags
            let projects = matchItemsToTags projects (fun x -> x.Id) projectTags

            return! htmlView (Views.index games books projects ctx) next ctx
        }