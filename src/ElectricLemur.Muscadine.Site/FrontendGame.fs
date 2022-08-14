module ElectricLemur.Muscadine.Site.Frontend.Game
open Giraffe
open Giraffe.ViewEngine
open Microsoft.AspNetCore.Http;
open ElectricLemur.Muscadine.Site

let getInProgress ctx =
  Tag.itemIdsContainingTags Game.documentType [ "in-progress" ] ctx
  |> Task.bind (fun ids -> Database.getDocumentsById Database.idField ids Database.Filters.empty ctx)
  |> Task.map (Seq.map Game.makeModelFromJObject)

let getBacklog ctx =
  Tag.itemIdsContainingTags Game.documentType [ "backlog" ] ctx
  |> Task.bind (fun ids -> Database.getDocumentsById Database.idField ids Database.Filters.empty ctx)
  |> Task.map (Seq.map Game.makeModelFromJObject)

let getOther (excluding: seq<seq<Game.Game>>) ctx = task {
  let ids = excluding |> Seq.flatten |> Seq.map (fun g -> g.Id)

  let filter =
    Database.Filters.empty
    |> Database.Filters.byDocumentType Game.documentType

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
    |> Seq.map Game.makeModelFromJObject
    |> Seq.filter (fun g -> not (hashSet.Contains(g.Id)))

  return others
}

let makeItemCard ctx (item: Game.Game) = task {
  let jobj = item |> Game.makeJObjectFromModel |> Some
  let title = item.Name
  let icon = jobj |> Items.readItemImageOrDefault
  let link = jobj |> Items.tryReadSlug |> Option.bind (fun slug -> Items.getLinkToItem Game.documentType slug ctx)

  let! mostRecentMicroblog =
    Microblog.loadMostRecentMicroblogForItem item.Id ctx
    |> Task.map (Option.map (fun mb -> mb.Microblog.DateAdded, mb.Microblog.Text))

  let! tags = Tag.loadTagsForDocument Game.documentType item.Id ctx

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
      let makeAndSortCards (games: seq<Game.Game>) =
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

  let GET_itemPage slug : HttpHandler =
    fun next (ctx: HttpContext) -> task {
      let! item = Items.tryLookupBySlug slug Game.documentType Game.makeModelFromJObject ctx

      let! content =
        item
        |> Option.map (fun item ->
            let icon = match item.CoverImagePaths with
                       | Some paths -> Image.Icon.Image paths
                       | None -> Items.getDefaultIcon Game.documentType

            icon, item)
        |> Option.mapAsync (fun (icon, item) -> task {
          let! microblogEntries = Microblog.loadMicroblogsForDocument Game.documentType item.Id ctx
          return (icon, microblogEntries, item)
        })
        |> Task.bind (Option.mapAsync (fun (icon, microblogEntries, item) -> task {
          let! tags = Tag.loadTagsForDocument Game.documentType item.Id ctx
          return (icon, tags, microblogEntries, item)
        }))
        |> Task.map (Option.map (fun (icon, tags, microblogEntries, item) ->
            FrontendHelpers.makeItemPage item.Name item.Description icon tags microblogEntries ctx))

      match content with
      | None -> return! (setStatusCode 404 >=> text "Page not found") next ctx
      | Some content ->
          let pageHtml = FrontendHelpers.layout FrontendHelpers.PageDefinitions.Games content [ "frontend/item_page.scss" ] ctx
          return! (htmlView pageHtml next ctx)

    }
