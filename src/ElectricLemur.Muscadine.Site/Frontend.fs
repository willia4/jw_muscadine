module ElectricLemur.Muscadine.Site.Frontend

open Giraffe
open Giraffe.ViewEngine
open Microsoft.AspNetCore.Http;

module PageDefinitions =
  type Page =
    | AboutMe
    | Projects
    | Books
    | Games
    | Colophon

  let sidebarButtonTitle page =
    match page with
    | AboutMe -> "About Me", "About Me"
    | Projects -> "What Am I Working On?", "Projects"
    | Books -> "What Am I Reading?", "Books"
    | Games -> "What Am I Playing?", "Games"
    | Colophon -> "Colophon", "Colophon"

  let sidebarButtonIcon page =
    match page with
    | AboutMe -> Some (i [ _class "fa-solid fa-user" ] [])
    | Projects -> Some (i [ _class "fa-solid fa-laptop-code" ] [])
    | Books -> Some (i [ _class "fa-solid fa-book-open" ] [])
    | Games -> Some (i [ _class "fa-solid fa-gamepad" ] [])
    | Colophon -> Some (i [ _class "fa-solid fa-pen-nib" ] [])

  let pageTitle = sidebarButtonTitle >> fst

  let pageRoute page =
    match page with
    | AboutMe -> "/"
    | Projects -> "/projects/"
    | Books -> "/books/"
    | Games -> "/games/"
    | Colophon -> "/colophon/"

  let makeSidebarButton currentPage buttonPage =
    let title = sidebarButtonTitle buttonPage
    let active = currentPage = buttonPage
    let largeButtonClass = if active then "sidebar-button large-button active" else "sidebar-button large-button"
    let smallButtonClass = if active then "sidebar-button small-button active" else "sidebar-button small-button"

    let buttonIcon =
      match sidebarButtonIcon buttonPage with
      | Some i -> [ i ]
      | None -> []

    [
        a [ (_class largeButtonClass); (_href (pageRoute buttonPage)) ] (List.append buttonIcon [ encodedText (fst title) ])
        a [ (_class smallButtonClass); (_href (pageRoute buttonPage)) ] (List.append buttonIcon [ encodedText (snd title) ])
    ]


let layout pageDefinition content =
  let pageHeader = PageDefinitions.pageTitle pageDefinition
  let sidebarOrder = [ PageDefinitions.AboutMe; PageDefinitions.Projects; PageDefinitions.Books; PageDefinitions.Games; PageDefinitions.Colophon ]

  html []
    [
      head [] [
                meta [ (_name "viewport"); (_content "width=device-width, initial-scale=1") ]
                meta [ (_httpEquiv "Content-Type"); (_content "text/html; charset=utf-8") ]
                title [] [ encodedText $"James Williams.me - %s{ pageHeader }" ]
                link [ (_rel "stylesheet"); (_type "text/css"); (_href "/css/remedy.css") ]
                link [ (_rel "stylesheet"); (_type "text/css"); (_href "/css/frontend.css") ]
                script [ (_src "https://kit.fontawesome.com/84935c491f.js"); (_crossorigin "anonymous") ] []
                script [ (_src "/js/main.js") ] []
            ]
      body [] [
        div [ _class "content-wrapper" ] [
          div [ _class "logo"] [ img [ _src "/img/head_logo_512.png" ] ]
          div [ _class "header" ] [
            button [ (_class "menu-button"); (attr "@click" "alert('Hello World!')") ] [
              i [ _class "fa-solid fa-bars" ] []
            ]
            encodedText pageHeader
          ]
          div [ _class "sidebar"] [
            ul [] (
              sidebarOrder
              |> List.map (fun p ->
                  li [] (PageDefinitions.makeSidebarButton pageDefinition p ))

            )
          ]
          div [ _class "main-content" ] content
        ]
      ]
    ]


let indexHandler =
  fun next (ctx: HttpContext) ->
    let lineCount =
      match ctx.GetQueryStringValue "lines" with
      | Ok lines -> Some lines
      | _ -> None
      |> Option.bind (fun x -> match System.Int32.TryParse(x) with
                               | true, i -> Some i
                               | false, _ -> None)
      |> Option.defaultValue 100

    let lines = [1..lineCount] |> List.map (fun i -> div [] [ encodedText $"Line %d{i}" ])
    layout PageDefinitions.AboutMe [
      div [ ] lines
    ]
    |> (fun x -> htmlView x next ctx)
