module ElectricLemur.Muscadine.Site.ItemHelper
open Giraffe
open Giraffe.ViewEngine

type ItemWrapper =
  | Game of Game.Game
  | Project of Project.Project
  | Book of Book.Book

type ItemDocumentType =
  | GameDocumentType
  | ProjectDocumentType
  | BookDocumentType

module ItemDocumentType =
  let fromString s =
    match s with
    | s when s = Game.documentType -> Some GameDocumentType
    | s when s = Project.documentType -> Some ProjectDocumentType
    | s when s = Book.documentType -> Some BookDocumentType
    | _ -> None

let tryWrapItem (item: obj) =
  match item with
  | :? Game.Game as g -> Some (Game g)
  | :? Project.Project as p -> Some (Project p)
  | :? Book.Book as b -> Some (Book b)
  | _ -> None

let wrapItem (item: obj) =
  match tryWrapItem item with
  | Some i -> i
  | _ -> failwith $"Unable to unwrap type {item.GetType().FullName}"

let tryUnwrapGame (item) =
  match item with
  | Game g -> Some g
  | _ -> None

let tryUnwrapProject (item) =
  match item with
  | Project p -> Some p
  | _ -> None

let tryUnwrapBook (item) =
  match item with
  | Book b -> Some b
  | _ -> None

let unwrapGame = tryUnwrapGame >> Option.get
let unwrapProject = tryUnwrapProject >> Option.get
let unwrapBook = tryUnwrapBook >> Option.get

let fromJObject (obj: Newtonsoft.Json.Linq.JObject) =
    Some obj |> Option.choosef [
      Option.bind (Game.tryMakeModelFromJObject >> (Option.map Game))
      Option.bind (Project.tryMakeModelFromJObject >> (Option.map Project))
      Option.bind (Book.tryMakeModelFromJObject >> (Option.map Book))
    ]

let toJObject item =
  match item with
  | Game g -> Game.makeJObjectFromModel g
  | Project p -> Project.makeJObjectFromModel p
  | Book b -> Book.makeJObjectFromModel b

let fromContextForm itemDocumentType existing ctx =
  match ItemDocumentType.fromString itemDocumentType with
  | Some GameDocumentType ->
      let existing = existing |> Option.bind tryUnwrapGame
      Game.makeAndValidateModelFromContext existing ctx
      |> Task.map (Result.map wrapItem)
  | Some ProjectDocumentType ->
      let existing = existing |> Option.bind tryUnwrapProject
      Project.makeAndValidateModelFromContext existing ctx
      |> Task.map (Result.map wrapItem)
  | Some BookDocumentType ->
      let existing = existing |> Option.bind tryUnwrapBook
      Book.makeAndValidateModelFromContext existing ctx
      |> Task.map (Result.map wrapItem)
  | None -> Task.fromResult (Error $"Could not parse fields for document type %s{itemDocumentType}")

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

let githubLink item =
  match item with
  | Project p -> p.GitHubLink |> Option.map (fun url -> FrontendHelpers.GitHubLink url)
  | _ -> None

let itemLinks item =
  let appendItemLink link links =
    match link with
    | Some l -> List.append links [l]
    | None -> links

  []
  |> appendItemLink (githubLink item)

let pageDefinition item =
  match item with
  | Game g -> FrontendHelpers.PageDefinitions.Games
  | Project p -> FrontendHelpers.PageDefinitions.Projects
  | Book b -> FrontendHelpers.PageDefinitions.Books

let pageDefinitionForDocumentType itemDocumentType =
  match ItemDocumentType.fromString itemDocumentType with
  | Some GameDocumentType ->FrontendHelpers.PageDefinitions.Games
  | Some ProjectDocumentType -> FrontendHelpers.PageDefinitions.Projects
  | Some BookDocumentType -> FrontendHelpers.PageDefinitions.Books
  | None -> failwith $"Could not determine page definition for document type {itemDocumentType}"

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

let loadAllItems itemDocumentType ctx =
      Database.getDocumentsByType itemDocumentType Some Database.NoLimit ctx
      |> Task.map (Seq.map fromJObject)
      |> Task.map (Seq.filter Option.isSome)
      |> Task.map (Seq.map Option.get)


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

  module Admin =
    let addView itemDocumentType allTags =
      match ItemDocumentType.fromString itemDocumentType with
      | Some GameDocumentType -> Game.addEditView None allTags []
      | Some ProjectDocumentType -> Project.addEditView None allTags []
      | Some BookDocumentType -> Book.addEditView None allTags []
      | None -> failwith $"Could not determine addView for document type {itemDocumentType}"

    let editView item =
      match item with
      | Game g -> Game.addEditView (Some g)
      | Project p -> Project.addEditView (Some p)
      | Book b -> Book.addEditView (Some b)

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
          FrontendHelpers.makeItemPage (name item) (description item) (icon item) (itemLinks item) tags microblogEntries ctx))

      match content with
      | None -> return! (setStatusCode 404 >=> text "Page not found") next ctx
      | Some content ->
          let pageHtml = FrontendHelpers.layout FrontendHelpers.PageDefinitions.Games content [ "frontend/item_page.scss" ] ctx
          return! (htmlView pageHtml next ctx)
    }

module AdminHandlers =
  let private adminSlug item =
    match item with
    | Game _ -> "/admin/game/"
    | Project _ -> "/admin/project/"
    | Book _ -> "/admin/book/"

  let private coverImageKey item =
    match item with
    | Game _ -> Game.Fields.coverImagePaths.Key
    | Project _ -> Project.Fields.coverImagePaths.Key
    | Book _ -> Book.Fields.coverImagePaths.Key

  let private handleGameImageUpload item ctx =
    let unwrapped = (tryUnwrapGame >> Option.get) item
    Items.handleImageUpload ctx
      (documentType item) (itemId item)
      (coverImageKey item) (coverImages item)
      (fun n -> { unwrapped with CoverImagePaths = n})
    |> Task.map (Result.map wrapItem)

  let private handleProjectImageUpload item ctx =
    let unwrapped = (tryUnwrapProject >> Option.get) item
    Items.handleImageUpload ctx
      (documentType item) (itemId item)
      (coverImageKey item) (coverImages item)
      (fun n -> { unwrapped with IconImagePaths = n})
    |> Task.map (Result.map wrapItem)

  let private handleBookImageUpload item ctx =
    let unwrapped = (tryUnwrapBook >> Option.get) item
    Items.handleImageUpload ctx
      (documentType item) (itemId item)
      (coverImageKey item) (coverImages item)
      (fun n -> { unwrapped with CoverImagePaths = n})
    |> Task.map (Result.map wrapItem)

  let GET_add itemDocumentType : HttpHandler =
    fun next ctx -> 
      Tag.getExistingTags ctx
      |> Task.map (fun allTags -> Views.Admin.addView itemDocumentType allTags)
      |> Task.bind (fun view -> htmlView view next ctx)

  let POST_add itemDocumentType : HttpHandler =
    fun next ctx -> task {
      let! item = fromContextForm itemDocumentType None ctx

      match item with
      | Ok item ->
          let! coverImageUploadResult =
            match item with
            | Game _ -> handleGameImageUpload item ctx
            | Project _ -> handleProjectImageUpload item ctx
            | Book _ -> handleBookImageUpload item ctx

          match coverImageUploadResult with
          | Error msg -> return! (setStatusCode 400 >=> text msg) next ctx
          | Ok item ->
              let data = toJObject item
              do! Database.upsertDocument ctx data
              do! Tag.saveTagsForForm (documentType item) (itemId item) Tag.formKey ctx

              let redirectUrl = $"%s{adminSlug item}%s{itemId item}"
              return! (redirectTo false redirectUrl) next ctx
      | Error msg ->
          return! (setStatusCode 400 >=> text msg) next ctx
    }

  let GET_edit itemDocumentType id : HttpHandler =
    fun next ctx -> task {
      let! existing =
        Database.getDocumentById id ctx
        |> Task.map (Option.bind fromJObject)

      let! allTags = Tag.getExistingTags ctx
      let! documentTags = Tag.loadTagsForDocument itemDocumentType id ctx

      match existing with
      | Some existing ->
          let view = Views.Admin.editView existing allTags documentTags
          return! htmlView view next ctx
      | None ->
          return! (setStatusCode 404 >=> text "Page not found") next ctx
    }

  let POST_edit itemDocumentType id : HttpHandler =
    fun next ctx -> task {
      let! existing =
        Database.getDocumentById id ctx
        |> Task.map (Option.bind fromJObject)

      match existing with
      | None -> return! (setStatusCode 404) next ctx
      | Some existing ->
          let! newModel = fromContextForm itemDocumentType (Some existing) ctx
          match newModel with
          | Ok newModel ->
              let! coverImageUploadResult =
                match newModel with
                | Game _ -> handleGameImageUpload newModel ctx
                | Project _ -> handleProjectImageUpload newModel ctx
                | Book _ -> handleBookImageUpload newModel ctx

              match coverImageUploadResult with
              | Error msg -> return! (setStatusCode 400 >=> text msg) next ctx
              | Ok newModel ->
                  let data = toJObject newModel
                  do! Database.upsertDocument ctx data
                  do! Tag.saveTagsForForm (documentType newModel) (itemId newModel) Tag.formKey ctx

                  let redirectUrl = $"%s{adminSlug newModel}%s{itemId newModel}"
                  return! (redirectTo false redirectUrl) next ctx
          | Error msg -> return! (setStatusCode 400 >=> text msg) next ctx
    }

  let DELETE id : HttpHandler =
    fun next ctx -> task {
      let! existing =
        Database.getDocumentById id ctx
        |> Task.map (Option.bind fromJObject)

      let existingCoverImage = existing |> Option.bind coverImages

      do! match existingCoverImage with
          | Some existingCoverImage -> Image.deleteAllImages existingCoverImage ctx
          | None -> Task.fromResult ()

      do! match existing with
          | Some existing -> task {
              do! Tag.clearTagsForDocument (documentType existing) (itemId existing) ctx
              do! Microblog.deleteAllMicroblogsFromItem (documentType existing) (itemId existing) ctx
              do! Database.deleteDocument ctx (itemId existing)
            }
          | None -> Task.fromResult ()

      return! setStatusCode 200 next ctx
    }