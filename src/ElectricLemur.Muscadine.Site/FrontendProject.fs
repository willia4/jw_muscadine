module ElectricLemur.Muscadine.Site.Frontend.Project
open Giraffe
open Giraffe.ViewEngine
open Microsoft.AspNetCore.Http;
open ElectricLemur.Muscadine.Site

module Handlers =
  let GET_index : HttpHandler = ItemHelper.Handlers.Get_listIndex Project.documentType

  let GET_itemPage slug : HttpHandler = ItemHelper.Handlers.GET_itemPage Project.documentType slug
