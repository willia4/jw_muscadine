module ElectricLemur.Muscadine.Site.Frontend.Project
open Giraffe
open Giraffe.ViewEngine
open Microsoft.AspNetCore.Http;
open ElectricLemur.Muscadine.Site

let getInProgress ctx =
  Tag.itemIdsContainingTags Project.documentType [ "in-progress" ] ctx
  |> Task.bind (fun ids -> Database.getDocumentsById Database.idField ids Database.Filters.empty ctx)
  |> Task.map (Seq.map Project.makeModelFromJObject)

let getBacklog ctx =
  Tag.itemIdsContainingTags Project.documentType [ "backlog" ] ctx
  |> Task.bind (fun ids -> Database.getDocumentsById Database.idField ids Database.Filters.empty ctx)
  |> Task.map (Seq.map Project.makeModelFromJObject)

let getOther (excluding: seq<seq<Project.Project>>) ctx = task {
  let ids = excluding |> Seq.flatten |> Seq.map (fun p -> p.Id)

  let filter =
    Database.Filters.empty
    |> Database.Filters.byDocumentType Project.documentType

  // we should only do "not in" on around 30 or less
  let filter =
    ids
    |> Seq.truncate 30
    |> Seq.fold (fun filter id -> filter |> Database.Filters.addNotIn Database.idField id) filter

  let! others = Database.getDocumentsForFilter filter Database.NoLimit ctx

  // we weren't able to use the database to remove _all_ of the ids so we now have to laboriously check for any others
  let hashSet = new System.Collections.Generic.HashSet<string>()
  let others =
    others
    |> Seq.map Project.makeModelFromJObject
    |> Seq.filter (fun p -> not (hashSet.Contains(p.Id)))

  return others
}

let makeItemCard ctx (item: Project.Project) = task {
  let jobj = item |> Project.makeJObjectFromModel |> Some
  let title = item.Name
  let icon = jobj |> Items.readItemImageOrDefault
  let link = jobj |> Items.tryReadSlug |> Option.bind (fun slug -> Items.getLinkToItem Project.documentType slug ctx)

  let! mostRecentMicroblog =
    Microblog.loadMostRecentMicroblogForItem item.Id ctx
    |> Task.map (Option.map (fun mb -> mb.Microblog.DateAdded, mb.Microblog.Text))

  let! tags = Tag.loadTagsForDocument Project.documentType item.Id ctx

  let sortDate = mostRecentMicroblog |> Option.map fst |> Option.defaultValue System.DateTimeOffset.MinValue
  let card = FrontendHelpers.makeItemCard title link tags mostRecentMicroblog icon ctx
  return  sortDate, card
}

module Views =
  let makeContentView inProgressCards backlogCards otherCards =
    ([
      section [ _id "in-progress-section" ] [
        div [ _class "in-progress-container item-card-container" ] inProgressCards
      ]
    ])
    |> List.appendIf (backlogCards |> (Seq.isEmpty >> not)) (
        section [ _id "backlog-section" ] [
          h2 [ ] [ encodedText "Backlog" ]
          div [ _class "backlog-container item-card-container" ] backlogCards
        ])
    |> List.appendIf (otherCards |> (Seq.isEmpty >> not)) (
      section [ _id "other-section" ] [
        h2 [ ] [ encodedText "Other" ]
        div [ _class "other-container item-card-container" ] otherCards
      ])

module Handlers =
  let GET_index =
    fun next (ctx: HttpContext) ->
      let makeAndSortCards (games: seq<Project.Project>) =
        games
        |> Seq.mapAsync (makeItemCard ctx)
        |> Task.map (Seq.sortByDescending fst)
        |> Task.map (Seq.map snd)
        |> Task.map (Seq.toList)

      task {
        let! inProgress = getInProgress ctx
        let! inProgressCards = inProgress |> makeAndSortCards


        let! backlog = getBacklog ctx
        let! backlogCards = backlog |> makeAndSortCards

        let! others = getOther [inProgress; backlog] ctx
        let! otherCards = others |> makeAndSortCards

        let content = Views.makeContentView inProgressCards backlogCards otherCards

        let pageHtml = FrontendHelpers.layout FrontendHelpers.PageDefinitions.Projects content ["frontend/item_cards.scss"] ctx

        return! htmlView pageHtml next ctx
          }

  let GET_itemPage slug : HttpHandler = ItemHelper.Handlers.GET_itemPage Project.documentType slug
