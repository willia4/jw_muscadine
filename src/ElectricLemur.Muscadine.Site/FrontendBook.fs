module ElectricLemur.Muscadine.Site.Frontend.Book
open Giraffe
open ElectricLemur.Muscadine.Site

module Handlers =
  let GET_index : HttpHandler = ItemHelper.Handlers.Get_listIndex ItemHelper.ItemDocumentType.BookDocumentType

  let GET_itemPage slug : HttpHandler = ItemHelper.Handlers.GET_itemPage ItemHelper.ItemDocumentType.BookDocumentType slug