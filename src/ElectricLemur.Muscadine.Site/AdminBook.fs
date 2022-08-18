module ElectricLemur.Muscadine.Site.Admin.Book
open Giraffe
open ElectricLemur.Muscadine.Site

module Handlers =
  let GET_add : HttpHandler = ItemHelper.AdminHandlers.GET_add Book.documentType
  let GET_edit id : HttpHandler = ItemHelper.AdminHandlers.GET_edit Book.documentType id

  let POST_add : HttpHandler = ItemHelper.AdminHandlers.POST_add Book.documentType
  let POST_edit id : HttpHandler = ItemHelper.AdminHandlers.POST_edit Book.documentType id