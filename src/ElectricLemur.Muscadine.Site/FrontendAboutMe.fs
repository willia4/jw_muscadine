module ElectricLemur.Muscadine.Site.Frontend.AboutMe
open ElectricLemur.Muscadine.Site.FrontendHelpers
open Giraffe
open Giraffe.ViewEngine
open Microsoft.AspNetCore.Http
open ElectricLemur.Muscadine.Site

let makeMicroblogsContent (recentMicroblogs: Microblog.EnrichedMicroblog seq) ctx =
  recentMicroblogs
  |> Seq.map (fun mb ->
    let d = mb.Microblog.DateAdded.ToString("o")
    let markdownHtml = Markdig.Markdown.ToHtml(mb.Microblog.Text)

    let icon = Image.xmlElementFromIcon mb.ItemIcon ImagePaths.choose256 ctx

    div [ _class "microblog" ] [
      FrontendHelpers.wrapNodeInLinkIfHrefExists mb.Link (div [ _class "icon" ] [
        icon
      ])

      div [ _class "microblog-text-container" ] [
        div [ _class "header" ] [
          yield span [ _class "header-text" ] [ FrontendHelpers.wrapNodeInLinkIfHrefExists mb.Link (encodedText mb.ItemName) ]

          yield match Microblog.permalink mb ctx with
                | Some permalink ->
                    span [ _class "timestamp" ] [
                      a [ _href permalink] [
                        script [] [ rawText $"document.write(formatUtcDate(\"%s{d}\"));" ]
                        noscript [] [ encodedText d ]
                      ]
                    ]
                | None ->
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

let aboutMeContent recentMicroblogs ctx =
  let biographyParagraphs = Util.extractEmbeddedTextFile "biography.html"

  biographyParagraphs, [
    main [ _class "page-content about-me" ] [
      div [ _class "about-text-container" ]  [
        div [ _class "header" ] [
          header [] [
            div [ _class "subtitle" ] [ encodedText "Hello, I am"]
            h1 [ _class "title" ] [ encodedText "James Williams"]
          ]
          div [ _class "about-photo-container"] [
            img [ _src "/img/james_and_gary.jpg" ]
          ]
        ]
        div [ _class "biography" ] [ rawText biographyParagraphs ]
        div [ _class "buttons" ] [
          // a [ (_href "https://www.facebook.com/willia4"); ( attr "aria-label" "Facebook" )] [ i [ _class "fa-brands fa-facebook-f"] []]
          a [ (_href "https://github.com/willia4"); ( attr "aria-label" "Github" )] [ i [ _class "fa-brands fa-github"] []]
          a [ (_href "https://social.lol/@willia4"); ( attr "aria-label" "Mastodon" )] [ i [ _class "fa-brands fa-mastodon"] []]
          a [ (_href "https://www.twitter.com/willia4"); ( attr "aria-label" "Twitter" )] [ i [ _class "fa-brands fa-twitter"] []]
          a [ (_href "https://photos.jameswilliams.me"); ( attr "aria-label" "Photos" )] [ i [ _class "fa-solid fa-camera"] []]
          a [ (_href "https://www.linkedin.com/in/jameswilliams-me/"); ( attr "aria-label" "Linked In" )] [ i [ _class "fa-brands fa-linkedin"] []]
          a [ (_href "https://app.thestorygraph.com/profile/willia4"); ( attr "aria-label" "StoryGraph") ] [ i [ _class "fa-solid fa-book-open" ] []]
          a [ (_href "https://www.backloggd.com/u/willia4/"); ( attr "aria-label" "Backloggd") ] [ i [ _class "fa-solid fa-gamepad" ] []]
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
      div [ _class "microblogs-container" ] (makeMicroblogsContent recentMicroblogs ctx)
      div [ _class "microblogs-see-all" ] [
        a [ _href "/updates/" ] [
          i [ _class "fa-solid fa-angles-right" ] []
          encodedText "All Activity"
          ]
      ]
    ]
  ]

let allMicroblogsContent microblogs ctx = [
  main [ _class "page-content all-microblogs" ] [
    div [ _class "page-content recent-activity" ] [
      div [ _class "microblogs-container" ] (makeMicroblogsContent microblogs ctx)
    ]
  ]
]

module Handlers =
  let GET_index =
    fun next (ctx: HttpContext) -> task {
      let! recentMicroblogs = Microblog.loadRecentMicroblogs (System.DateTimeOffset.UtcNow) (Database.Limit 7) ctx

      let (contentAsHtml, contentAsNodes) = aboutMeContent recentMicroblogs ctx
      
      let openGraphMetaData = {
          Title = Some "James Williams.me - About Me"
          Description = Some (Util.extractTextFromHtml contentAsHtml)
          ImageUrl = Some (System.Uri("https://jameswilliams.me/img/james_and_gary.jpg"))
          Labels = NoLabel 
        }
      
      let pageHtml = FrontendHelpers.layout FrontendHelpers.PageDefinitions.AboutMe contentAsNodes [ FrontendHelpers.PageExtra.CSS  "frontend/about_me.scss" ] FrontendHelpers.NoPageData openGraphMetaData None ctx

      return! htmlView pageHtml next ctx
    }

  let GET_all =
    fun next (ctx: HttpContext) -> task {
      let! recentMicroblogs = Microblog.loadRecentMicroblogs (System.DateTimeOffset.UtcNow) (Database.NoLimit) ctx
      let content = allMicroblogsContent recentMicroblogs ctx
      let pageHtml = FrontendHelpers.layout
                       (FrontendHelpers.PageDefinitions.Custom ("updates", "All Updates", "All Updates", FrontendHelpers.PageDefinitions.AboutMe, []) )
                       content
                       [ FrontendHelpers.PageExtra.CSS "frontend/about_me.scss" ]
                       FrontendHelpers.NoPageData
                       OpenGraphMetadata.empty
                       None
                       ctx

      return! htmlView pageHtml next ctx
    }

  let GET_allForItemType itemDocumentTypeSlug : HttpHandler =
    fun next (ctx: HttpContext) -> task {
      match ItemHelper.ItemDocumentType.fromSlug itemDocumentTypeSlug with
      | Some itemDocumentType ->
        let databaseDocumentType = ItemHelper.ItemDocumentType.toDatabaseDocumentType itemDocumentType
        let slugString = ItemHelper.ItemDocumentType.toSlug itemDocumentType

        let! recentMicroblogs = Microblog.loadRecentMicroblogsForItemType (System.DateTimeOffset.UtcNow) databaseDocumentType Database.NoLimit ctx
        let content = allMicroblogsContent recentMicroblogs ctx

        let pluralTitle = ItemHelper.ItemDocumentType.toPluralTitleString itemDocumentType
        let singularTitle = ItemHelper.ItemDocumentType.toSingularTitleString itemDocumentType

        let longTitle = $"All Updates - %s{pluralTitle}"
        let shortTitle = $"%s{singularTitle} Updates"

        let activeButtonPage =
          match itemDocumentType with
          | ItemHelper.ItemDocumentType.GameDocumentType -> FrontendHelpers.PageDefinitions.Games
          | ItemHelper.ItemDocumentType.ProjectDocumentType -> FrontendHelpers.PageDefinitions.Projects
          | ItemHelper.ItemDocumentType.BookDocumentType -> FrontendHelpers.PageDefinitions.Books
          | ItemHelper.ItemDocumentType.ImageLibraryRecordDocumentType -> FrontendHelpers.PageDefinitions.Games

        let pageHtml = FrontendHelpers.layout
                         (FrontendHelpers.PageDefinitions.Custom ($"updates/%s{slugString}", longTitle, shortTitle, activeButtonPage, []))
                         content
                         [ FrontendHelpers.PageExtra.CSS  "frontend/about_me.scss" ]
                         FrontendHelpers.NoPageData
                         OpenGraphMetadata.empty
                         None
                         ctx

        return! htmlView pageHtml next ctx
      | None ->
        return! (setStatusCode 404 >=> text "Page Not Found") next ctx
    }
