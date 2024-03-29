module ElectricLemur.Muscadine.Site.FrontendHelpers
open Giraffe.ViewEngine

module PageDefinitions =
  type Page =
    | AboutMe
    | Projects
    | Books
    | Games
    | Colophon
    | Custom of slug: string * longName: string * shortName: string * selectedPage: Page * customHeader: (XmlNode list)

  let sidebarButtonTitle page =
    match page with
    | AboutMe -> "About Me", "About Me"
    | Projects -> "What Am I Working On?", "Projects"
    | Books -> "What Am I Reading?", "Books"
    | Games -> "What Am I Playing?", "Games"
    | Colophon -> "Colophon", "Colophon"
    | Custom (_, longName, shortName, _, _) -> longName, shortName

  let sidebarButtonIcon page =
    match page with
    | AboutMe -> Some (i [ _class Constants.Icons.AboutMe ] [])
    | Projects -> Some (i [ _class Constants.Icons.Project ] [])
    | Books -> Some (i [ _class Constants.Icons.Book ] [])
    | Games -> Some (i [ _class Constants.Icons.Game ] [])
    | Colophon -> Some (i [ _class Constants.Icons.Colophon ] [])
    | Custom _ -> None

  let pageTitle page =
    match page with
    | Colophon -> "About This Site"
    | _ -> sidebarButtonTitle page |> fst

  let customHeader page =
    match page with
    | Custom (_, _, _, _, customHeader) -> customHeader
    | _ -> []
    
  let pageRoute page =
    match page with
    | AboutMe -> "/about/"
    | Projects -> "/projects/"
    | Books -> "/books/"
    | Games -> "/games/"
    | Colophon -> "/colophon/"
    | Custom (slug, _, _, _, _) -> $"/%s{slug}/"

  type SidebarButton = {
    LongTitle: string
    ShortTitle: string
    Active: bool
    Route: string
    Icon: XmlNode option
  }

  module SidebarButton =
    module DefaultButtons =
      let AboutMe = {
        LongTitle = "About Me"
        ShortTitle = "About Me"
        Route = "/about/"
        Icon = Some (i [ _class Constants.Icons.AboutMe ] [])
        Active = false
      }
      
      let Projects = {
        LongTitle = "What Am I Working On?"
        ShortTitle = "Projects"
        Route = "/projects/"
        Icon = Some (i [ _class Constants.Icons.Project ] [])
        Active= false
      }
      
      let Books = {
        LongTitle = "What Am I Reading?"
        ShortTitle = "Books"
        Route = "/books/"
        Icon = Some (i [ _class Constants.Icons.Book ] [])
        Active = false
      }
      
      let Games = {
        LongTitle = "What Am I Playing?"
        ShortTitle = "Games"
        Route = "/games/"
        Icon = Some (i [ _class Constants.Icons.Game ] [])
        Active = false
      }
      
      let Colophon = {
        LongTitle = "Colophon"
        ShortTitle = "Colophon"
        Route = "/colophon/"
        Icon = Some (i [ _class Constants.Icons.Colophon ] [])
        Active = false
      }
    let defaultSidebar (currentPage: Page option) =
      [
        { DefaultButtons.AboutMe with Active = currentPage |> Option.map (fun p -> p = AboutMe) |> Option.defaultValue false }
        { DefaultButtons.Projects with Active = currentPage |> Option.map (fun p -> p = Projects) |> Option.defaultValue false }
        { DefaultButtons.Books with Active = currentPage |> Option.map (fun p -> p = Books) |> Option.defaultValue false }
        { DefaultButtons.Games with Active = currentPage |> Option.map (fun p -> p = Games) |> Option.defaultValue false }
        { DefaultButtons.Colophon with Active = currentPage |> Option.map (fun p -> p = Colophon) |> Option.defaultValue false }
      ]
      
      
  let makeSidebarButton (button: SidebarButton) =
    let title = (button.LongTitle, button.ShortTitle)

    let largeButtonClass = if button.Active then "sidebar-button large-button active" else "sidebar-button large-button"
    let smallButtonClass = if button.Active then "sidebar-button small-button active" else "sidebar-button small-button"

    let buttonIcon =
      match button.Icon with
      | Some i -> [ i ]
      | None -> []

    [
        a [ (_class largeButtonClass); (_href button.Route); (attr "role" "menuitem") ] (List.append buttonIcon [ encodedText (fst title) ])
        a [ (_class smallButtonClass); (_href button.Route); (attr "role" "menuitem") ] (List.append buttonIcon [ encodedText (snd title) ])
    ]

type ItemLink =
  | GitHubLink of string

type PageExtra =
  | CSS of string
  | JavaScript of string

type PageDataType =
    | String of value: string
    | Int of value: int
    | Float of value: float
    
type PageData =
  | PageData of Map<string, PageDataType>
  | NoPageData
  
type OpenGraphLabels =
  | NoLabel
  | OneLabel of (string * string)
  | TwoLabels of ((string * string) * (string * string))
  
module OpenGraphLabels =
  let label1 = function | OneLabel t -> Some t | TwoLabels (t, _) -> Some t | _ -> None
  let label2 = function | TwoLabels (_, t) -> Some t | _ -> None
  
  let label1Text l = l |> label1 |> Option.map fst
  let label1Value l = l |> label1 |> Option.map fst
  let label2Text l = l |> label2 |> Option.map fst
  let label2Value l = l |> label2 |> Option.map fst
 
  let toMetaNodes l =
    match l with
    | OneLabel (l1_text, l1_value) -> [
        meta [(_name "twitter:label1"); (_value l1_text)]
        meta [(_name "twitter:data1"); (_value l1_value)]
      ]
    | TwoLabels ((l1_text, l1_value), (l2_text, l2_value)) -> [
        meta [(_name "twitter:label1"); (_value l1_text)]
        meta [(_name "twitter:data1"); (_value l1_value)]
        meta [(_name "twitter:label2"); (_value l2_text)]
        meta [(_name "twitter:data2"); (_value l2_value)]
      ]
    | NoLabel -> []
    
type OpenGraphMetadata = {
  Title: string option
  Description: string option
  ImageUrl: System.Uri option
  Labels: OpenGraphLabels
}

module OpenGraphMetadata =
  let empty = {
    Title = None
    Description = None
    ImageUrl = None
    Labels =  NoLabel
  }
  
  let toMetaNodes md =[
      yield! match md.Title with
               | Some t when (not (System.String.IsNullOrWhiteSpace(t))) ->
                  let t = t.Trim()
                  [
                    meta [(_property "og:title"); (_content t)]
                    meta [(_name "twitter:title"); (_content t)]
                  ]
               | _ -> []
        
      yield! match md.Description with
              | Some d when (not (System.String.IsNullOrWhiteSpace(d))) ->
                  let d = System.Text.RegularExpressions.Regex.Replace(input = d, pattern = "\n+", replacement = " ")
                  let d = System.Text.RegularExpressions.Regex.Replace(input = d, pattern = @"\s+", replacement = " ")
                  let d = d.Trim()
                  
                  let d = if d.Length > 160 then
                            let d' = d.Substring(0, 156)
                            $"{d'}..."
                          else d

                  [
                    meta [(_property "og:description"); (_content d)]
                    meta [(_name "twitter:description"); (_content d)]
                    meta [(_name "description"); (_content d)]
                  ]
              | _ -> []
              
      yield! match md.ImageUrl with
              | Some u -> [
                  meta [(_property "og:image"); (_content (string u)) ]
                  meta [(_name "twitter:image"); (_content (string u))]
                ]
              | None -> []
                
      yield! (OpenGraphLabels.toMetaNodes md.Labels)
    ]

let layout pageDefinition content extraCss (pageData: PageData) (openGraphMetadata: OpenGraphMetadata) (sidebar: Option<PageDefinitions.SidebarButton list>) ctx =
  let homeUrl = Util.baseUrl ctx
  let pageHeader = PageDefinitions.pageTitle pageDefinition
  let sidebarOrder = [ PageDefinitions.AboutMe; PageDefinitions.Projects; PageDefinitions.Books; PageDefinitions.Games; PageDefinitions.Colophon ]

  let sidebar =
    match sidebar with
    | Some sidebar -> sidebar
    | None -> PageDefinitions.SidebarButton.defaultSidebar (Some pageDefinition)
    
  let pageDataScript =
    let sb = System.Text.StringBuilder("window.pageData = {};")

    ( match pageData with
      | NoPageData -> sb
      | PageData pageData ->
          for k in (Map.keys pageData) do
            sb
              .Append($"window.pageData[\"{k}\"] = ")
              .Append(match pageData[k] with
                      | String s -> $"\"{s}\""
                      | Int i -> $"{i}"
                      | Float f -> $"{f}")
              .AppendLine(";")
            |> ignore
          sb
    ).ToString()

  let headNodes =
    [
        meta [ (_httpEquiv "Content-Type"); (_content "text/html; charset=utf-8") ]
        meta [ (_name "viewport"); (_content "width=device-width, initial-scale=1") ]
        link [ (_rel "shortcut icon"); (_type "image/png"); (_href "/img/head_logo_32.png") ]
        link [ (_rel "icon"); (_type "image/png"); (_href "/img/head_logo_256.png") ]
        link [ (_rel "me"); ( _href "https://social.lol/@willia4") ]
        link [ (_rel "me"); ( _href "https://forlorn.computer/@willia4") ]

        link [ (_rel "alternate"); (_type "application/atom+xml"); (_title "All Microblogs - Atom Feed"); (_href (Util.makeUrl "/feed/microblogs/" ctx |> string))]
        link [ (_rel "alternate"); (_type "application/atom+xml"); (_title "Book Microblogs - Atom Feed"); (_href (Util.makeUrl "/feed/microblogs/books/" ctx |> string))]
        link [ (_rel "alternate"); (_type "application/atom+xml"); (_title "Project Microblogs - Atom Feed"); (_href (Util.makeUrl "/feed/microblogs/projects/" ctx |> string))]
        link [ (_rel "alternate"); (_type "application/atom+xml"); (_title "Game Microblogs - Atom Feed"); (_href (Util.makeUrl "/feed/microblogs/games/" ctx |> string))]

        title [] [ encodedText $"James Williams.me - %s{ pageHeader }" ]
        (Util.cssLinkTag "remedy.css" ctx)
        (Util.cssLinkTag "frontend.scss" ctx)
        (Util.cssLinkTag "print.scss" ctx)
        script [] [ rawText pageDataScript ]
        script [ (_src "https://kit.fontawesome.com/84935c491f.js"); (_crossorigin "anonymous") ] []
        (Util.javascriptTag "main.js" ctx)
    ]
    |> Seq.prepend (
      extraCss
      |> Seq.map (fun extra ->
                    match extra with
                    | CSS css -> Util.cssLinkTag css ctx
                    | JavaScript js -> Util.javascriptTag js ctx))
    |> Seq.append (OpenGraphMetadata.toMetaNodes openGraphMetadata)
    |> Seq.toList

  html []
    [
      head [] headNodes
      body [] [
        div [ _id "content-wrapper" ] [
          div [ _id "main-logo"] [
            a [ _href homeUrl ] [
              img [ (_src "/img/head_logo_512.png"); (_alt "Site logo") ]
              ]
          ]
          header [ _id "main-header" ] [
            a [ (_href homeUrl); (_class "mini-logo") ] [
              img [ (_src "/img/head_logo_512.png"); (_alt "Site Logo"); (_class "mini-logo") ]
            ]

            encodedText pageHeader
            
            yield! (PageDefinitions.customHeader pageDefinition)

            button [ (_class "menu-button"); ] [
              i [ _class "fa-solid fa-bars" ] []
            ]
          ]
          div [ _id "main-sidebar"] [
            ul [ attr "role" "menu" ] (
              sidebar
              |> List.map (fun button ->
                  li [ (attr "role" "presentation") ] (PageDefinitions.makeSidebarButton button ))

            )
          ]
          div [ _id "main-content" ] content
        ]
      ]
    ]

let wrapNodeInLinkIfHrefExists href node =
  match href with
  | Some href -> a [ _href href ] [ node ]
  | None -> node

let makeItemCard title link tags (microblog: (System.DateTimeOffset * string) option) (image: Image.Icon) ctx =
  let tags = Tag.sortTagsForDisplay tags

  let microblogDiv =
    match microblog with
    | Some (date, text) ->
        let date = date.ToString("o")
        let markdownHtml = Markdig.Markdown.ToHtml(text)

        div [ _class "item-microblog"] [
          div [ _class "item-microblog-heading" ] [
              span [ _class "item-microblog-heading-text"] [ rawText "Most Recent Update" ]
              span [ _class "date" ] [
                script [] [ rawText $"document.write(formatUtcDate(\"%s{date}\"));" ]
                noscript [] [ encodedText date ]
            ]
          ]
          span [ _class "text"] [ rawText markdownHtml ]
        ]
    | None ->
      div [ _class "item-microblog" ] [
        div [ _class "item-microblog-heading" ] [
          span [ _class "no-update" ] [ encodedText "No Updates Yet..." ]
        ]
      ]

  let tagsDiv =
    div [ _class "item-tags" ] (
      tags |> List.map (fun t ->
        span [ _class "item-tag" ] [ encodedText t ]))


  div [ _class "item-card" ] [
    div [ _class "item-image"] [
      div [ _class "item-image-container" ] [
        wrapNodeInLinkIfHrefExists link (Image.xmlElementFromIcon image ImagePaths.choose512 ctx)
      ]
    ]
    div [ _class "item-text-container" ] ([
      div [ _class "item-title" ] [ wrapNodeInLinkIfHrefExists link (encodedText title) ]
    ]
    |> List.prepend [ tagsDiv ]
    |> List.prepend [ microblogDiv ])
  ]


let makeItemPage title titleLink (subtitle: XmlNode option) (description: string) icon itemLinks tags microblogEntries ctx =

  let makeMicroblogContent (microblogEntries: Microblog.EnrichedMicroblog seq) ctx =
    microblogEntries
    |> Seq.map (fun mb ->
        let d = mb.Microblog.DateAdded.ToString("o")
        let markdownHtml = Markdig.Markdown.ToHtml(mb.Microblog.Text)

        div [ _class "microblog" ] [
          div [ _class "microblog-text-container" ] [
            div [ _class "header" ] [
              yield! match Microblog.permalink mb ctx with
                     | Some permaLink ->
                       [
                         a [ _href permaLink ] [
                            i [ _class "fa-solid fa-diamond" ] []

                            span [ _class "timestamp" ] [
                              script [] [ rawText $"document.write(formatUtcDate(\"%s{d}\"));" ]
                              noscript [] [ encodedText d ]
                            ]
                          ]
                       ]
                     | None -> [
                            i [ _class "fa-solid fa-diamond" ] []

                            span [ _class "timestamp" ] [
                              script [] [ rawText $"document.write(formatUtcDate(\"%s{d}\"));" ]
                              noscript [] [ encodedText d ]
                            ]
                     ]
            ]
            div [ _class "text" ] [ rawText markdownHtml]
          ]
        ])
    |> Seq.toList

  let iconNode = Image.xmlElementFromIcon icon ImagePaths.choose1024 ctx

  let tags = Tag.sortTagsForDisplay tags
  let tagsDiv =
    div [ _class "item-tags" ] (
      tags |> List.map (fun t ->
        span [ _class "item-tag" ] [ encodedText t ]))

  let itemLinks =
    itemLinks
    |> Seq.map (
        function
        | GitHubLink url ->
            div [ _class "item-link" ] [
              a [ _href url ] [
                i [ _class "fa-brands fa-github" ] []
                encodedText "Browse on GitHub"
              ]
            ])
    |> Seq.toList
    |> fun itemLinks ->
        if itemLinks |> List.isEmpty then
          None
        else
          Some (div [ _class "item-links-container" ] itemLinks)

  let descriptionDiv = div [ _class "item-description" ] [
    if subtitle |> Option.isSome then
      let subtitle = subtitle |> Option.get
      yield div [ _class "item-subtitle" ] [ subtitle ]

    yield rawText (Markdig.Markdown.ToHtml(description))
  ]

  [
    yield div [ (_class "page-content item"); (_id "desktop-content") ] [
        div [ _class "item-text-container" ] [
          yield h1 [ _class "title" ] [
            yield match titleLink with
                  | Some titleLink -> a [ _href titleLink ] [ encodedText title ]
                  | None -> encodedText title
          ]
          if Option.isSome itemLinks then yield (itemLinks |> Option.get)
          yield tagsDiv
          yield descriptionDiv
        ]

        div [ _class "item-photo-container" ] [
          iconNode
        ]
      ]

    yield div [ (_class "page-content item"); (_id "phone-content") ] [
        div [ _class "phone-header" ] [
          div [ _class "phone-header-image" ] [ iconNode]
          div [ _class "item-text-container" ] [
            yield h1 [ _class "title" ] [ encodedText title ]
            if Option.isSome itemLinks then yield (itemLinks |> Option.get)
            yield tagsDiv
          ]
        ]
        div [ _class "phone-description" ] [
          descriptionDiv
        ]
      ]

    if microblogEntries |> Option.isSome then
      let microblogEntries = microblogEntries |> Option.get
      yield div [ _class "page-content activity" ] [
          h2 [ _class "activity-title" ] [ encodedText "Activity" ]
          div [ _class "microblogs-container" ] (makeMicroblogContent microblogEntries ctx)
        ]
  ]