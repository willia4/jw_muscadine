module ElectricLemur.Muscadine.Site.Admin.Project
open Giraffe
open Giraffe.ViewEngine
open Microsoft.AspNetCore.Http;
open ElectricLemur.Muscadine.Site

module Handlers =
  let GET_add : HttpHandler = ItemHelper.AdminHandlers.GET_add Project.documentType
  let GET_edit id : HttpHandler = ItemHelper.AdminHandlers.GET_edit Project.documentType id

  let POST_add : HttpHandler = ItemHelper.AdminHandlers.POST_add Project.documentType
  let POST_edit id : HttpHandler = ItemHelper.AdminHandlers.POST_edit Project.documentType id