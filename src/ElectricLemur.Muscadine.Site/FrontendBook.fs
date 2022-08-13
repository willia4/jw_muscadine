module ElectricLemur.Muscadine.Site.Frontend.Book
open Giraffe
open Giraffe.ViewEngine
open Microsoft.AspNetCore.Http;
open ElectricLemur.Muscadine.Site

let getInProgress ctx =
  Tag.itemIdsContainingTags Book.documentType [ "in-progress" ] ctx
  |> Task.bind (fun ids -> Database.getDocumentsById Database.idField ids Database.Filters.empty ctx)
  |> Task.map (Seq.map Book.makeModelFromJObject)

let getBacklog ctx =
  Tag.itemIdsContainingTags Book.documentType [ "backlog" ] ctx
  |> Task.bind (fun ids -> Database.getDocumentsById Database.idField ids Database.Filters.empty ctx)
  |> Task.map (Seq.map Book.makeModelFromJObject)

let getOther (excluding: seq<seq<Book.Book>>) ctx = task {
  let ids = excluding |> Seq.flatten |> Seq.map (fun b -> b.Id)

  let filter =
    Database.Filters.empty
    |> Database.Filters.byDocumentType Book.documentType

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
    |> Seq.map Book.makeModelFromJObject
    |> Seq.filter (fun b -> not (hashSet.Contains(b.Id)))

  return others
}

let makeItemCard ctx (item: Book.Book) = task {
  let title = item.Title
  let icon = item |> Book.makeJObjectFromModel |> Some |> Items.readItemImageOrDefault

  let! mostRecentMicroblog =
    Microblog.loadMostRecentMicroblogForItem item.Id ctx
    |> Task.map (Option.map (fun mb -> mb.Microblog.DateAdded, mb.Microblog.Text))

  let sortDate = mostRecentMicroblog |> Option.map fst |> Option.defaultValue System.DateTimeOffset.MinValue
  let card = FrontendHelpers.makeItemCard title mostRecentMicroblog icon
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
      let makeAndSortCards (games: seq<Book.Book>) =
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

        let pageHtml = FrontendHelpers.layout FrontendHelpers.PageDefinitions.Games content ["frontend/item_cards.scss"] ctx

        return! htmlView pageHtml next ctx
          }