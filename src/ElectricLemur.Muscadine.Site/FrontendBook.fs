module ElectricLemur.Muscadine.Site.Frontend.Book
open Giraffe
open Giraffe.ViewEngine
open Microsoft.AspNetCore.Http;
open ElectricLemur.Muscadine.Site

module Handlers =
  let GET_index : HttpHandler = ItemHelper.Handlers.Get_listIndex Book.documentType

  let GET_itemPage slug : HttpHandler = ItemHelper.Handlers.GET_itemPage Book.documentType slug