module ElectricLemur.Muscadine.Site.ItemHelper
open Giraffe
open Giraffe.ViewEngine

type ItemWrapper =
  | Game of Game.Game
  | Project of Project.Project
  | Book of Book.Book

let tryWrapItem (item: obj) =
  match item with
  | :? Game.Game as g -> Some (Game g)
  | :? Project.Project as p -> Some (Project p)
  | :? Book.Book as b -> Some (Book b)
  | _ -> None

let wrapItem (item: obj) =
  match tryWrapItem obj with
  | Some i -> i
  | _ -> failwith $"Unable to unwrap type {item.GetType().FullName}"

let fromJObject obj =
    Database.documentTypeField |> JObj.getter<string> obj
    |> Option.bind (
        function
        | s when s = Game.documentType -> Some (Game.makeModelFromJObject obj |> Game)
        | s when s = Project.documentType -> Some (Project.makeModelFromJObject obj |> Project)
        | s when s = Book.documentType -> Some (Book.makeModelFromJObject obj |> Book)
        | s -> None
    )

let toJObject item =
  match item with
  | Game g -> Game.makeJObjectFromModel g
  | Project p -> Project.makeJObjectFromModel p
  | Book b -> Book.makeJObjectFromModel b

let documentType item =
  match item with
  | Game _ -> Game.documentType
  | Project _ -> Project.documentType
  | Book _ -> Book.documentType

let itemId item =
  match item with
  | Game g -> g.Id
  | Project p -> p.Id
  | Book b -> b.Id

let name item =
  match item with
  | Game g -> g.Name
  | Project p -> p.Name
  | Book b -> b.Title

let description item =
  match item with
  | Game g -> g.Description
  | Project p -> p.Description
  | Book b -> b.Description

let dateAdded item =
  match item with
  | Game g -> g.DateAdded
  | Project p -> p.DateAdded
  | Book b -> b.DateAdded

let coverImages item =
  match item with
  | Game g -> g.CoverImagePaths
  | Project p -> p.IconImagePaths
  | Book b -> b.CoverImagePaths

let icon item =
  match coverImages item with
  | Some coverImages -> Image.Icon.Image coverImages
  | None -> Items.getDefaultIcon (documentType item)

let slug item =
  match item with
  | Game g -> g.Slug
  | Project p -> p.Slug
  | Book b -> b.Slug

let pageDefinition item =
  match item with
  | Game g -> FrontendHelpers.PageDefinitions.Games
  | Project p -> FrontendHelpers.PageDefinitions.Projects
  | Book b -> FrontendHelpers.PageDefinitions.Books

let pageDefinitionForDocumentType itemDocumentType =
  match itemDocumentType with
  | s when s = Game.documentType ->FrontendHelpers.PageDefinitions.Games
  | s when s = Project.documentType -> FrontendHelpers.PageDefinitions.Projects
  | s when s = Book.documentType -> FrontendHelpers.PageDefinitions.Books
  | _ -> failwith $"Could not determine page definition for document type {itemDocumentType}"

let loadItemsContainingTags (itemDocumentType: string) tags ctx =
  Tag.itemIdsContainingTags itemDocumentType tags ctx
  |> Task.bind (fun ids -> Database.getDocumentsById Database.idField ids Database.Filters.empty ctx)
  |> Task.map (Seq.map fromJObject)
  |> Task.map (Seq.filter Option.isSome)
  |> Task.map (Seq.map Option.get)

let loadItemsExcludingItems (itemDocumentType: string) excludedItems ctx = task {
  let ids = excludedItems |> Seq.map itemId
  let filter =
    Database.Filters.empty
    |> Database.Filters.byDocumentType itemDocumentType

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
    |> Seq.map fromJObject
    |> Seq.filter Option.isSome
    |> Seq.map Option.get
    |> Seq.filter (fun item -> not (hashSet.Contains(itemId item)))

  return others
}

let loadInProgressItems itemDocumentType = loadItemsContainingTags itemDocumentType [ "in-progress" ]
let loadBacklogItems itemDocumentType = loadItemsContainingTags itemDocumentType [ "backlog" ]

let makeItemCard ctx item = task {
  let jObj = item |> toJObject |> Some
  let title = (name item)
  let icon = (icon item)
  let link = Items.getLinkToItem (documentType item) (slug item) ctx

  let! mostRecentMicroblog =
    Microblog.loadMostRecentMicroblogForItem (itemId item) ctx
    |> Task.map (Option.map (fun mb -> mb.Microblog.DateAdded, mb.Microblog.Text))

  let! tags = Tag.loadTagsForDocument (documentType item) (itemId item) ctx
  let sortDate = mostRecentMicroblog |> Option.map fst |> Option.defaultValue System.DateTimeOffset.MinValue
  let card = FrontendHelpers.makeItemCard title link tags mostRecentMicroblog icon ctx
  return sortDate, card
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
  let Get_listIndex itemDocumentType : HttpHandler =
    fun next ctx ->
      let makeAndSortCards (items: seq<ItemWrapper>) =
        items
        |> Seq.mapAsync (makeItemCard ctx)
        |> Task.map (Seq.sortByDescending fst)
        |> Task.map (Seq.map snd)
        |> Task.map (Seq.toList)

      task {
        let! inProgress = loadInProgressItems itemDocumentType ctx
        let! inProgressCards = inProgress |> makeAndSortCards

        let! backlog = loadBacklogItems itemDocumentType ctx
        let! backlogCards = backlog |> makeAndSortCards

        let! others = loadItemsExcludingItems itemDocumentType (Seq.append inProgress backlog) ctx
        let! otherCards = others |> makeAndSortCards

        let content = Views.makeContentView inProgressCards backlogCards otherCards
        let pageHtml = FrontendHelpers.layout (pageDefinitionForDocumentType itemDocumentType) content [ "frontend/item_cards.scss" ] ctx

        return! htmlView pageHtml next ctx
      }

  let GET_itemPage itemDocumentType slug : HttpHandler =
    fun next ctx -> task {
      let! item =
        Items.tryLookupBySlug slug itemDocumentType id ctx
        |> Task.map (Option.bind fromJObject)

      let! content =
        item
        |> Option.mapAsync (fun item -> task {
          let! microblogEntries = Microblog.loadMicroblogsForDocument (documentType item) (itemId item) ctx
          return (microblogEntries, item)
        })
        |> Task.bind (Option.mapAsync (fun (microblogEntries, item) -> task {
          let! tags = Tag.loadTagsForDocument (documentType item) (itemId item) ctx
          return (tags, microblogEntries, item)
        }))
        |> Task.map (Option.map (fun (tags, microblogEntries, item) ->
        FrontendHelpers.makeItemPage (name item) (description item) (icon item) tags microblogEntries ctx))

      match content with
      | None -> return! (setStatusCode 404 >=> text "Page not found") next ctx
      | Some content ->
          let pageHtml = FrontendHelpers.layout FrontendHelpers.PageDefinitions.Games content [ "frontend/item_page.scss" ] ctx
          return! (htmlView pageHtml next ctx)
    }