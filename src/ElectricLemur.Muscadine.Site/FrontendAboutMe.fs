module ElectricLemur.Muscadine.Site.Frontend.AboutMe
open Giraffe
open Giraffe.ViewEngine
open Microsoft.AspNetCore.Http
open Newtonsoft.Json.Linq
open ElectricLemur.Muscadine.Site

let makeMicroblogsContent (recentMicroblogs: (string * Image.Icon * System.DateTimeOffset * string * (string * string * JObject option)) seq) =
  recentMicroblogs
  |> Seq.map (fun (name, icon, date, text, itemInfo) ->
    let d = date.ToString("o")
    let markdownHtml = Markdig.Markdown.ToHtml(text)

    let icon =
      match icon with
      | Image.FontAwesome iconClass -> i [ _class iconClass ] []
      | Image.UrlPath path -> img [ _src path ]

    div [ _class "microblog" ] [
      a [ _class "icon" ] [
        icon
      ]
      div [ _class "microblog-text-container" ] [
        div [ _class "header" ] [
          span [ _class "header-text" ] [ encodedText name ]

          span [ _class "timestamp" ] [
            script [] [ rawText $"document.write(formatUtcDate(\"%s{d}\"));" ]
            noscript [] [ encodedText d ]
          ]
        ]
        div [ _class "text "] [ rawText markdownHtml ]
      ]
    ]
  )
  |> List.ofSeq

let aboutMeContent recentMicroblogs =
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

    div [ _class "page-content recent-activity" ] [
      h2 [ _class "activity-title" ] [ encodedText "My Recent Activity" ]
      div [ _class "microblogs-container" ] (makeMicroblogsContent recentMicroblogs)
    ]
  ]

module Handlers =
  let GET_index =
    fun next (ctx: HttpContext) -> task {
      let lineCount =
        match ctx.GetQueryStringValue "lines" with
        | Ok lines -> Some lines
        | _ -> None
        |> Option.bind (fun x -> match System.Int32.TryParse(x) with
                                 | true, i -> Some i
                                 | false, _ -> None)
        |> Option.defaultValue 100

      let! recentMicroblogs = Microblog.loadRecentMicroblogs (System.DateTimeOffset.UtcNow) (Database.Limit 7) ctx
      let recentMicroblogs =
        recentMicroblogs
        |> Seq.map (fun (name, icon, mb, itemInfo) ->

          name, icon, mb.DateAdded, mb.Text, itemInfo
        )

      let content = aboutMeContent recentMicroblogs
      let pageHtml = FrontendHelpers.layout FrontendHelpers.PageDefinitions.AboutMe content [ "frontend/about_me.scss" ] ctx

      return! htmlView pageHtml next ctx
    }