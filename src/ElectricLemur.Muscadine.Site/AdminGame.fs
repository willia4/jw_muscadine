module ElectricLemur.Muscadine.Site.Admin.Game
open Giraffe
open Giraffe.ViewEngine
open Microsoft.AspNetCore.Http;
open ElectricLemur.Muscadine.Site

module Handlers =
  let GET_add : HttpHandler = ItemHelper.AdminHandlers.GET_add Game.documentType
  let GET_edit id : HttpHandler = ItemHelper.AdminHandlers.GET_edit Game.documentType id

  let POST_add : HttpHandler = ItemHelper.AdminHandlers.POST_add Game.documentType
  let POST_edit id : HttpHandler = ItemHelper.AdminHandlers.POST_edit Game.documentType id

  