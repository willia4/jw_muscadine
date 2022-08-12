module ElectricLemur.Muscadine.Site.FrontendHelpers

open Giraffe
open Giraffe.ViewEngine
open Microsoft.AspNetCore.Http;
open Newtonsoft.Json.Linq

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
    | AboutMe -> Some (i [ _class Constants.Icons.AboutMe ] [])
    | Projects -> Some (i [ _class Constants.Icons.Project ] [])
    | Books -> Some (i [ _class Constants.Icons.Book ] [])
    | Games -> Some (i [ _class Constants.Icons.Game ] [])
    | Colophon -> Some (i [ _class Constants.Icons.Colophon ] [])

  let pageTitle = sidebarButtonTitle >> fst

  let pageRoute page =
    match page with
    | AboutMe -> "/about/"
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
        a [ (_class largeButtonClass); (_href (pageRoute buttonPage)); (attr "role" "menuitem") ] (List.append buttonIcon [ encodedText (fst title) ])
        a [ (_class smallButtonClass); (_href (pageRoute buttonPage)); (attr "role" "menuitem") ] (List.append buttonIcon [ encodedText (snd title) ])
    ]


let layout pageDefinition content extraCss ctx =
  let pageHeader = PageDefinitions.pageTitle pageDefinition
  let sidebarOrder = [ PageDefinitions.AboutMe; PageDefinitions.Projects; PageDefinitions.Books; PageDefinitions.Games; PageDefinitions.Colophon ]

  let headNodes =
    [
        meta [ (_httpEquiv "Content-Type"); (_content "text/html; charset=utf-8") ]
        meta [ (_name "viewport"); (_content "width=device-width, initial-scale=1") ]
        link [ (_rel "shortcut icon"); (_type "image/png"); (_href "/img/head_logo_32.png") ]
        link [ (_rel "icon"); (_type "image/png"); (_href "/img/head_logo_256.png") ]
        title [] [ encodedText $"James Williams.me - %s{ pageHeader }" ]
        (Util.cssLinkTag "remedy.css" ctx)
        (Util.cssLinkTag "frontend.scss" ctx)
        script [ (_src "https://kit.fontawesome.com/84935c491f.js"); (_crossorigin "anonymous") ] []
        (Util.javascriptTag "main.js" ctx)
    ]
    |> Seq.prepend (extraCss |> Seq.map (fun css -> Util.cssLinkTag css ctx))
    |> Seq.toList

  html []
    [
      head [] headNodes
      body [] [
        div [ _id "content-wrapper" ] [
          div [ _id "main-logo"] [ img [ (_src "/img/head_logo_512.png"); (_alt "Site logo") ] ]
          header [ _id "main-header" ] [
            img [ (_src "/img/head_logo_512.png"); (_alt "Site Logo"); (_class "mini-logo") ]
            encodedText pageHeader
            button [ (_class "menu-button"); ] [
              i [ _class "fa-solid fa-bars" ] []
            ]
          ]
          div [ _id "main-sidebar"] [
            ul [ attr "role" "menu" ] (
              sidebarOrder
              |> List.map (fun p ->
                  li [ (attr "role" "presentation") ] (PageDefinitions.makeSidebarButton pageDefinition p ))

            )
          ]
          div [ _id "main-content" ] content
        ]
      ]
    ]