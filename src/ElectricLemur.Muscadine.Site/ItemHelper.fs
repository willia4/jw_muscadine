module ElectricLemur.Muscadine.Site.ItemHelper
open ElectricLemur.Muscadine.Site.FrontendHelpers
open Giraffe
open Giraffe.ViewEngine

type ItemWrapper =
  | Game of Game.Game
  | Project of Project.Project
  | Book of Book.Book
  | Image of Image.ImageLibraryRecord

type ItemDocumentType =
  | GameDocumentType
  | ProjectDocumentType
  | BookDocumentType
  | ImageLibraryRecordDocumentType

module ItemDocumentType =
  let fromString s =
    match s with
    | s when s = Game.documentType -> Some GameDocumentType
    | s when s = Project.documentType -> Some ProjectDocumentType
    | s when s = Book.documentType -> Some BookDocumentType
    | s when s = ImageLibraryRecord.documentType -> Some ImageLibraryRecordDocumentType
    | _ -> None

  let fromSlug s =
    match s with
    | s when s = "games" -> Some GameDocumentType
    | s when s = "projects" -> Some ProjectDocumentType
    | s when s = "books" -> Some BookDocumentType
    | s when s = "images" -> Some ImageLibraryRecordDocumentType
    | _ -> None

  let toDatabaseDocumentType itemDocumentType =
    match itemDocumentType with
    | GameDocumentType -> Game.documentType
    | ProjectDocumentType -> Project.documentType
    | BookDocumentType -> Book.documentType
    | ImageLibraryRecordDocumentType -> ImageLibraryRecord.documentType

  let toSlug itemDocumentType =
    match itemDocumentType with
    | GameDocumentType -> "games"
    | ProjectDocumentType -> "projects"
    | BookDocumentType -> "books"
    | ImageLibraryRecordDocumentType -> "images"

  let toPluralTitleString itemDocumentType =
    match itemDocumentType with
    | GameDocumentType -> "Games"
    | ProjectDocumentType -> "Projects"
    | BookDocumentType -> "Books"
    | ImageLibraryRecordDocumentType -> "Images"

  let toSingularTitleString itemDocumentType =
    match itemDocumentType with
    | GameDocumentType -> "Game"
    | ProjectDocumentType -> "Project"
    | BookDocumentType -> "Book"
    | ImageLibraryRecordDocumentType -> "Image"
    
let tryWrapItem (item: obj) =
  match item with
  | :? Game.Game as g -> Some (Game g)
  | :? Project.Project as p -> Some (Project p)
  | :? Book.Book as b -> Some (Book b)
  | :? Image.ImageLibraryRecord as i -> Some (Image i)
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

let tryUnwrapImage (item) =
  match item with
  | Image i -> Some i
  | _ -> None
  
let unwrapGame = tryUnwrapGame >> Option.get
let unwrapProject = tryUnwrapProject >> Option.get
let unwrapBook = tryUnwrapBook >> Option.get
let unwrapImage = tryUnwrapImage >> Option.get

let fromJObject (obj: Newtonsoft.Json.Linq.JObject) =
    Some obj |> Option.choosef [
      Option.bind (Game.tryMakeModelFromJObject >> (Option.map Game))
      Option.bind (Project.tryMakeModelFromJObject >> (Option.map Project))
      Option.bind (Book.tryMakeModelFromJObject >> (Option.map Book))
      Option.bind (ImageLibraryRecord.fromJObject >> (Option.map Image))
    ]

let toJObject item =
  match item with
  | Game g -> FormFields.makeJObjectFromModel g Game.documentType Game.Fields.allFields
  | Project p -> FormFields.makeJObjectFromModel p Project.documentType Project.Fields.allFields
  | Book b -> FormFields.makeJObjectFromModel b Book.documentType Book.Fields.allFields
  | Image i -> ImageLibraryRecord.toJObject i

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
  | Some ImageLibraryRecordDocumentType ->
      let existing = existing |> Option.bind tryUnwrapImage
      ImageLibraryRecord.makeAndValidateModelFromContext existing ctx
      |> Task.map (Result.map wrapItem)
  | None -> Task.fromResult (Error $"Could not parse fields for document type %s{itemDocumentType}")

let documentType item =
  match item with
  | Game _ -> Game.documentType
  | Project _ -> Project.documentType
  | Book _ -> Book.documentType
  | Image _ -> ImageLibraryRecord.documentType

let itemId item =
  match item with
  | Game g -> g.Id
  | Project p -> p.Id
  | Book b -> b.Id
  | Image i -> i.Id

let name item =
  match item with
  | Game g -> g.Name
  | Project p -> p.Name
  | Book b -> b.Title
  | Image i -> i.Name

let description item =
  match item with
  | Game g -> g.Description
  | Project p -> p.Description
  | Book b -> b.Description
  | Image _ -> ""

let dateAdded item =
  match item with
  | Game g -> g.DateAdded
  | Project p -> p.DateAdded
  | Book b -> b.DateAdded
  | Image i -> i.DateAdded

let coverImages item =
  match item with
  | Game g -> g.CoverImagePaths
  | Project p -> p.IconImagePaths
  | Book b -> b.CoverImagePaths
  | Image i -> ImageLibraryRecord.getImagePaths i |> Some

let icon item =
  match coverImages item with
  | Some coverImages -> Image.Icon.Image coverImages
  | None -> Items.getDefaultIcon (documentType item)

let slug item =
  match item with
  | Game g -> g.Slug
  | Project p -> p.Slug
  | Book b -> b.Slug
  | Image i -> i.Id

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
  | Game _ -> FrontendHelpers.PageDefinitions.Games
  | Project _ -> FrontendHelpers.PageDefinitions.Projects
  | Book _ -> FrontendHelpers.PageDefinitions.Books
  | Image _ -> failwith "There is no frontend page definition available for images"

let pageDefinitionForDocumentType itemDocumentType =
  match itemDocumentType with
  | GameDocumentType ->FrontendHelpers.PageDefinitions.Games
  | ProjectDocumentType -> FrontendHelpers.PageDefinitions.Projects
  | BookDocumentType -> FrontendHelpers.PageDefinitions.Books
  | ImageLibraryRecordDocumentType -> failwith "There is no frontend page definition available for images"

let loadItemsContainingTags itemDocumentType tags ctx =
  Tag.itemIdsContainingTags (ItemDocumentType.toDatabaseDocumentType itemDocumentType) tags ctx
  |> Task.bind (fun ids -> Database.getDocumentsById Database.idField ids Database.Filters.empty ctx)
  |> Task.map (Seq.map fromJObject)
  |> Task.map (Seq.filter Option.isSome)
  |> Task.map (Seq.map Option.get)

let loadItemsExcludingItems itemDocumentType excludedItems ctx = task {
  let ids = excludedItems |> Seq.map itemId
  let filter =
    Database.Filters.empty
    |> Database.Filters.byDocumentType (ItemDocumentType.toDatabaseDocumentType itemDocumentType)

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

let loadFinishedItems itemDocumentType ctx = task {
  let finishedTags = [ "finished"; "read"; "done" ]
  let tasks =
    finishedTags
    |> List.map (fun t -> loadItemsContainingTags itemDocumentType [ t ] ctx)
  let! finishedTasks  = System.Threading.Tasks.Task.WhenAll(tasks)
  return
    finishedTasks
    |> Seq.collect id
    |> Seq.distinctBy itemId
}

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

let tryLookupItemBySlug (slug: string) documentType  ctx =
  let filter =
      Database.Filters.empty
      |> Database.Filters.byDocumentType documentType
      |> Database.Filters.addEquals "slug" slug

  Database.getDocumentsForFilter filter (Database.Limit 1) ctx
  |> Task.map Seq.tryHead
  |> Task.map (Option.bind fromJObject)


module Views =
  let makeContentView itemDocumentType inProgressCards backlogCards finishedCards otherCards =
    let notEmpty = List.isEmpty >> not

    let sections = div [ _class "sections" ] [
      if (notEmpty inProgressCards) then
        yield section [ _id "in-progress-section" ] [
                div [ _class "in-progress-container item-card-container" ] inProgressCards
              ]

      if (notEmpty backlogCards) then
        yield section [ _id "backlog-section" ] [
                h2 [ ] [ encodedText "Backlog" ]
                div [ _class "backlog-container item-card-container" ] backlogCards
              ]

      if (notEmpty finishedCards) then
        yield section [ _id "finished-section" ] [
                h2 [  ] [ encodedText "Finished" ]
                div [ _class "backlog-container item-card-container" ] finishedCards
        ]

      if (notEmpty otherCards) then
        yield section [ _id "other-section" ] [
                h2 [ ] [ encodedText "Other" ]
                div [ _class "other-container item-card-container" ] otherCards
              ]
    ]

    [
      yield sections

      let slug = ItemDocumentType.toSlug itemDocumentType
      yield div [ _class "microblogs-see-all" ] [
              a [ _href $"/updates/%s{slug}" ] [
                i [ _class "fa-solid fa-angles-right" ] []
                encodedText "All Activity"
                ]
            ]
    ]

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

        let! finished = loadFinishedItems itemDocumentType ctx
        let! finishedCards = finished |> makeAndSortCards
        
        let! others = loadItemsExcludingItems itemDocumentType (Seq.flatten [inProgress; backlog; finished]) ctx
        let! otherCards = others |> makeAndSortCards

        let content = Views.makeContentView itemDocumentType inProgressCards backlogCards finishedCards otherCards
        let pageHtml = FrontendHelpers.layout (pageDefinitionForDocumentType itemDocumentType) content [ PageExtra.CSS "frontend/item_cards.scss" ] NoPageData OpenGraphMetadata.empty None ctx

        return! htmlView pageHtml next ctx
      }

  let GET_itemPage itemDocumentType slug : HttpHandler =
    fun next ctx -> task {
      let! item = tryLookupItemBySlug slug (ItemDocumentType.toDatabaseDocumentType itemDocumentType) ctx

      let! processedContent =
        item
        |> Option.mapAsync (fun item -> task {
          let! microblogEntries = Microblog.loadEnrichedMicroblogsForDocument (documentType item) (toJObject item |> Some) ctx
          return (microblogEntries, item)
        })
        |> Task.bind (Option.mapAsync (fun (microblogEntries, item) -> task {
          let! tags = Tag.loadTagsForDocument (documentType item) (itemId item) ctx
          return (tags, microblogEntries, item)
        }))
        |> Task.map (Option.map (fun (tags, microblogEntries, item) ->
          let microblogEntries = Some microblogEntries
          let itemLink = Items.getLinkToItem (documentType item) slug ctx
          item, FrontendHelpers.makeItemPage (name item) itemLink None (description item) (icon item) (itemLinks item) tags microblogEntries ctx))

      match processedContent with
      | Some (item, content) ->
          let openGraphMetadata = {
            Title = Some ($"James Williams.me - {name item}")
            Description = Some (description item)
            ImageUrl = (coverImages item |> Option.bind (fun icon -> Image.uriFromIcon icon ImagePaths.choose256 ctx)) 
            Labels = NoLabel 
          }
          let pageHtml = FrontendHelpers.layout (pageDefinition item) content [ PageExtra.CSS "frontend/item_page.scss" ] NoPageData openGraphMetadata None ctx
          return! (htmlView pageHtml next ctx)
      | _ -> return! (setStatusCode 404 >=> text "Page not found") next ctx
    }

  let GET_microblogPage itemDocumentType slug microblogId : HttpHandler =
    fun next ctx -> task {
      let! item = tryLookupItemBySlug slug (ItemDocumentType.toDatabaseDocumentType itemDocumentType) ctx
      let! microblog = Microblog.loadById microblogId ctx

      match microblog, item with
      | Some microblog, Some item when (microblog.ItemId) = (itemId item) ->
        let! tags = Tag.loadTagsForDocument (documentType item) (itemId item) ctx
        let itemLink = Items.getLinkToItem (documentType item) slug ctx
        let microblogLink = Microblog.permalink microblog ctx

        let subtitle =
          match microblogLink with
          | Some microblogLink ->
              Some (h3 [] [
                a [ _href microblogLink ] [
                  let d = microblog.Microblog.DateAdded.ToString("o")

                  yield encodedText "Activity: "
                  yield span [] [
                    script [] [ rawText $"document.write(formatUtcDate(\"%s{d}\"));" ]
                    noscript [] [ encodedText d ]
                  ]
                ]
              ])
          | None -> None
          
         
        // -4 hours is _roughly_ "my time" on the US East Coast and should broadly get the day right.
        // since that's all we're formatting, that's probably all that matters. 
        let openGraphDate =
          microblog.Microblog.DateAdded
            .ToOffset(System.TimeSpan.FromHours(-4))
            .ToString("yyyy-MMM-dd")
          
        let openGraphMetadata = { Title = Some ($"James Williams.me - {name item}")
                                  Description = (Some (Util.extractTextFromMarkdown microblog.Microblog.Text)) 
                                  ImageUrl = (coverImages item
                                              |> Option.bind (fun icon -> Image.uriFromIcon icon ImagePaths.choose256 ctx))
                                  Labels = OneLabel ("Posted On", openGraphDate) }
        
        let content = FrontendHelpers.makeItemPage (name item) itemLink subtitle microblog.Microblog.Text (icon item) [] tags None ctx
        let pageHtml = FrontendHelpers.layout (pageDefinition item) content [ PageExtra.CSS "frontend/item_page.scss" ] NoPageData openGraphMetadata None ctx
        return! (htmlView pageHtml next ctx)
      | _ -> return! (setStatusCode 404 >=> text "Page not found") next ctx
    }

module AdminHandlers =
  let private coverImageKey item =
    match item with
    | Game _ -> (FormFields.key Game.Fields.coverImagePaths)
    | Project _ -> (FormFields.key Project.Fields.coverImagePaths)
    | Book _ -> (FormFields.key Book.Fields.coverImagePaths)
    | Image _ -> (FormFields.key ImageLibraryRecord.Fields.imageData)

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

  let private handleImageLibraryImageUpload item ctx =
    let unwrapped = (tryUnwrapImage >> Option.get) item
    Items.handleImageUpload ctx
      (documentType item) (itemId item)
      (coverImageKey item) (coverImages item)
      (fun _ -> unwrapped)
    |> Task.map (Result.map wrapItem)

  let POST_add (itemDocumentType: ItemDocumentType) : HttpHandler =
    fun next ctx -> task {
      let! item = fromContextForm (ItemDocumentType.toDatabaseDocumentType itemDocumentType) None ctx

      match item with
      | Ok item ->
          let! coverImageUploadResult =
            match item with
            | Game _ -> handleGameImageUpload item ctx
            | Project _ -> handleProjectImageUpload item ctx
            | Book _ -> handleBookImageUpload item ctx
            | Image _ -> handleImageLibraryImageUpload item ctx

          match coverImageUploadResult with
          | Error msg -> return! (setStatusCode 400 >=> text msg) next ctx
          | Ok item ->
              let data = toJObject item
              do! Database.upsertDocument ctx data
              do! Tag.saveTagsForForm (documentType item) (itemId item) Tag.formKey ctx

              let redirectUrl = $"/admin/{ItemDocumentType.toSlug itemDocumentType}/{itemId item}"
              return! (redirectTo false redirectUrl) next ctx
      | Error msg ->
          return! (setStatusCode 400 >=> text msg) next ctx
    }



  let POST_edit (itemDocumentType: ItemDocumentType) id : HttpHandler =
    fun next ctx -> task {
      let! existing =
        Database.getDocumentById id ctx
        |> Task.map (Option.bind fromJObject)

      match existing with
      | None -> return! (setStatusCode 404) next ctx
      | Some existing ->
          let! newModel = fromContextForm (ItemDocumentType.toDatabaseDocumentType itemDocumentType) (Some existing) ctx
          match newModel with
          | Ok newModel ->
              let! coverImageUploadResult =
                match newModel with
                | Game _ -> handleGameImageUpload newModel ctx
                | Project _ -> handleProjectImageUpload newModel ctx
                | Book _ -> handleBookImageUpload newModel ctx
                | Image _ -> handleImageLibraryImageUpload newModel ctx

              match coverImageUploadResult with
              | Error msg -> return! (setStatusCode 400 >=> text msg) next ctx
              | Ok newModel ->
                  let data = toJObject newModel
                  do! Database.upsertDocument ctx data
                  do! Tag.saveTagsForForm (documentType newModel) (itemId newModel) Tag.formKey ctx

                  let redirectUrl = $"/admin/{ItemDocumentType.toSlug itemDocumentType}/{itemId newModel}"
                  return! (redirectTo false redirectUrl) next ctx
          | Error msg -> return! (setStatusCode 400 >=> text msg) next ctx
    }

  let DELETE id : HttpHandler =
    fun next ctx -> task {
      let! existing =
        Database.getDocumentById id ctx
        |> Task.map (Option.bind fromJObject)

      let existingLibraryImageRecord = existing |> Option.bind (tryUnwrapImage)
      let existingCoverImage = existing |> Option.bind coverImages

      do! match existingLibraryImageRecord, existingCoverImage with
          | Some existingLibraryImageRecord, _ -> Image.deleteFileSystemRecordsForImageLibraryRecord existingLibraryImageRecord ctx
          | _, Some existingCoverImage -> Image.deleteAllImagesForImagePaths existingCoverImage ctx
          | _, None -> Task.fromResult ()

      do! match existing with
          | Some existing -> task {
              do! Tag.clearTagsForDocument (documentType existing) (itemId existing) ctx
              do! Microblog.deleteAllMicroblogsFromItem (documentType existing) (itemId existing) ctx
              do! Database.deleteDocument ctx (itemId existing)
            }
          | None -> Task.fromResult ()

      return! setStatusCode 200 next ctx
    }