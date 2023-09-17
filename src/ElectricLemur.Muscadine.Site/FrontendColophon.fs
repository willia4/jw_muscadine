module ElectricLemur.Muscadine.Site.Frontend.Colophon
open ElectricLemur.Muscadine.Site.FrontendHelpers
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
      let pageHtml = FrontendHelpers.layout FrontendHelpers.PageDefinitions.Colophon content [ FrontendHelpers.PageExtra.CSS "frontend/colophon.scss" ] FrontendHelpers.NoPageData OpenGraphMetadata.empty None ctx

      return! htmlView pageHtml next ctx
    }