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

let aboutMeContent =
  let biographyParagraphs = Util.extractEmbeddedTextFile "biography.html"

  [
    main [ _class "page-content about-me" ] [
      div [ _class "about-text-container" ]  [
        header [] [
          div [ _class "subtitle" ] [ encodedText "Hello, I am"]
          h1 [ _class "title" ] [ encodedText "James Williams"]
        ]
        div [ _class "biography" ] [ rawText biographyParagraphs ]
        div [ _class "buttons" ] [
          // a [ (_href "https://www.facebook.com/willia4"); ( attr "aria-label" "Facebook" )] [ i [ _class "fa-brands fa-facebook-f"] []]
          a [ (_href "https://github.com/willia4"); ( attr "aria-label" "Github" )] [ i [ _class "fa-brands fa-github"] []]
          a [ (_href "https://www.twitter.com/willia4"); ( attr "aria-label" "Twitter" )] [ i [ _class "fa-brands fa-twitter"] []]
          a [ (_href "https://photos.jameswilliams.me"); ( attr "aria-label" "Photos" )] [ i [ _class "fa-solid fa-camera"] []]
          a [ (_href "https://www.linkedin.com/in/jameswilliams-me/"); ( attr "aria-label" "Linked In" )] [ i [ _class "fa-brands fa-linkedin"] []]
        ]
        div [ _class "resume-link"] [
          a [ (_href "https://jameswilliams.me/resume/"); (attr "aria-label" "Resume link" )] [
            i [ _class "fa-solid fa-angles-right" ] []
            encodedText "James' Resume"
          ]
        ]
      ]

      div [ _class "about-photo-container" ] [
        img [ _src "/img/james_and_gary.jpg" ]
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

    let content = aboutMeContent

    let pageHtml = layout PageDefinitions.AboutMe content [ "frontend/about_me.scss" ] ctx

    htmlView pageHtml next ctx
