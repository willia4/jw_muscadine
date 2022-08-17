module ElectricLemur.Muscadine.Site.ItemHelper
open Giraffe

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

module Handlers =
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