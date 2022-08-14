module ElectricLemur.Muscadine.Site.Frontend.Colophon
open Giraffe
open Giraffe.ViewEngine
open Microsoft.AspNetCore.Http;
open ElectricLemur.Muscadine.Site

let colophonContent =
  [
    main [ _class "page-content colophon" ] [
      div [ _class "text" ] [
        rawText (Util.extractEmbeddedTextFile "colophon.html")
      ]
    ]

  ]

module Handlers =
  let GET_index =
    fun next (ctx: HttpContext) -> task {

      let content = colophonContent
      let pageHtml = FrontendHelpers.layout FrontendHelpers.PageDefinitions.Colophon content [ "frontend/colophon.scss" ] ctx

      return! htmlView pageHtml next ctx
    }