module ElectricLemur.Muscadine.Site.Frontend.Book
open Giraffe
open Giraffe.ViewEngine
open Microsoft.AspNetCore.Http;
open ElectricLemur.Muscadine.Site

module Handlers =
  let GET_index =
    fun next (ctx: HttpContext) -> task {

      let content = []
      let pageHtml = FrontendHelpers.layout FrontendHelpers.PageDefinitions.Books content [] ctx

      return! htmlView pageHtml next ctx
    }
