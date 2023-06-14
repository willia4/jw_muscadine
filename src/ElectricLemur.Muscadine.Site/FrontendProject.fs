module ElectricLemur.Muscadine.Site.Frontend.Project
open Giraffe
open ElectricLemur.Muscadine.Site

module Handlers =
  let GET_index : HttpHandler = ItemHelper.Handlers.Get_listIndex ItemHelper.ItemDocumentType.ProjectDocumentType

  let GET_itemPage slug : HttpHandler = ItemHelper.Handlers.GET_itemPage ItemHelper.ItemDocumentType.ProjectDocumentType slug
