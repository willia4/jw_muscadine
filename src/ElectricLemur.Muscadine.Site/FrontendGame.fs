module ElectricLemur.Muscadine.Site.Frontend.Game
open Giraffe
open ElectricLemur.Muscadine.Site

module Handlers =
  let GET_index : HttpHandler = ItemHelper.Handlers.Get_listIndex ItemHelper.ItemDocumentType.GameDocumentType

  let GET_itemPage slug : HttpHandler = ItemHelper.Handlers.GET_itemPage ItemHelper.ItemDocumentType.GameDocumentType slug