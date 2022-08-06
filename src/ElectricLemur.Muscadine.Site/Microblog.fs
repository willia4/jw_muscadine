module Microblog
open ElectricLemur.Muscadine.Site
open System
open Giraffe
open Giraffe.ViewEngine
open Microsoft.AspNetCore.Http

let documentType = "microblog"

type private MicroblogAssignment = {
  Id: string
  DateAdded: DateTimeOffset
  ItemId: string
  ItemDocumentType: string
  Text: string
}

type Microblog = {
  Id: string
  DateAdded: DateTimeOffset
  Text: string
}

let sortMicroblogs (blogs: Microblog list) = blogs |> List.sortByDescending (fun b -> b.DateAdded)
let private sortMicroblogAssignments (blogs: MicroblogAssignment list) = blogs |> List.sortBy (fun b -> b.DateAdded)

let private MicroblogAssignmentToMicroblog (a: MicroblogAssignment) =
  {
    Id = a.Id
    DateAdded = a.DateAdded
    Text = a.Text
  }

let private microblogAssignmentToJObject (microblog: MicroblogAssignment) =
  new Newtonsoft.Json.Linq.JObject()
  |> JObj.setValue Database.idField microblog.Id
  |> JObj.setValue Database.documentTypeField documentType
  |> JObj.setValue "_dateAdded" microblog.DateAdded
  |> JObj.setValue AssociatedItem.itemIdField microblog.ItemId
  |> JObj.setValue AssociatedItem.itemDocumentTypeField microblog.ItemDocumentType
  |> JObj.setValue "text" microblog.Text

let private JObjectToMicroblogAssignment obj =
  let s k = JObj.getter<string> obj k |> Option.get
  let d k = JObj.getter<DateTimeOffset> obj k |> Option.get

  {
    Id = s Database.idField
    DateAdded = d "_dateAdded"
    ItemId = s AssociatedItem.itemIdField
    ItemDocumentType = s AssociatedItem.itemDocumentTypeField
    Text = s "text"
  }

let private microblogToJObject (microblog: Microblog) =
  new Newtonsoft.Json.Linq.JObject()
  |> JObj.setValue "_dateAdded" microblog.DateAdded
  |> JObj.setValue "id" microblog.Id
  |> JObj.setValue "text" microblog.Text

let private loadMicroblogAssignmentsForDocuments itemDocumentType itemIds ctx =
  AssociatedItem.loadAssociatedItemsForDocuments documentType itemDocumentType itemIds JObjectToMicroblogAssignment ctx

let private loadMicroblogAssignmentsForDocument itemDocumentType itemId ctx =
  loadMicroblogAssignmentsForDocuments itemDocumentType [ itemId ] ctx

let loadMicroblogsForDocuments itemDocumentType itemIds ctx = task {
  let! r = AssociatedItem.loadAssociatedItemMapForDocuments documentType itemDocumentType itemIds JObjectToMicroblogAssignment (fun i -> i.ItemId) MicroblogAssignmentToMicroblog ctx

  return r
    |> Map.map (fun _ blogs -> blogs |> sortMicroblogs)
}

let loadMicroblogsForDocument itemDocumentType itemId ctx = task {
  let! blogs = loadMicroblogsForDocuments itemDocumentType [itemId] ctx
  return blogs
    |> Map.tryFind itemId
    |> function
       | Some blogs -> blogs
       | None -> []
}

let private loadItemDataForMicroblogAssignment (microblogAssignment: MicroblogAssignment) ctx = task {
  let! item = Database.getDocumentByTypeAndId microblogAssignment.ItemDocumentType microblogAssignment.ItemId ctx
  let itemName =
    item
    |> Option.choosef [
      (fun obj -> Option.bind (fun obj -> JObj.getter<string> obj "name") obj)
      (fun obj -> Option.bind (fun obj -> JObj.getter<string> obj "title") obj)
    ]
    |> Option.defaultValue "Unknown"

  let itemIcon =
    match microblogAssignment.ItemDocumentType with
    | s when s = Constants.Database.DocumentTypes.Book -> Constants.Icons.Book
    | s when s = Constants.Database.DocumentTypes.Project -> Constants.Icons.Project
    | s when s = Constants.Database.DocumentTypes.Game -> Constants.Icons.Game
    | _ -> Constants.Icons.QuestionMark

  return itemName, itemIcon
}

let loadRecentMicroblogs (since: System.DateTimeOffset) limit ctx = task {
  let filter =
    Database.Filters.empty
    |> Database.Filters.byDocumentType documentType
    |> Database.Filters.addLessThanOrEqualTo "_dateAdded" (since.ToOffset(System.TimeSpan.Zero).ToString("o"))

  let sort =
    Database.Sort.empty
    |> Database.Sort.by "_dateAdded"

  let! documents = Database.getDocumentsForFilterAndSort filter sort limit ctx
  let documents =
    documents
    |> Seq.rev
    |> Seq.map JObjectToMicroblogAssignment

  let! documents =
    documents
    |> Seq.mapAsync (fun assignment -> task {
      let! (name, icon) = loadItemDataForMicroblogAssignment assignment ctx
      return (name, icon, MicroblogAssignmentToMicroblog assignment)
    })

  return documents
}

let addMicroblogToDocument itemDocumentType itemId text ctx = task {
  let microblogAssignment = {
    Id = Util.newGuid () |> string
    ItemDocumentType = itemDocumentType
    ItemId = itemId
    Text = text
    DateAdded = DateTimeOffset.UtcNow
  }

  let! _ = Database.insertDocument ctx (microblogAssignmentToJObject microblogAssignment)
  return ()
}

let deleteAllMicroblogsFromItem itemDocumentType itemId ctx = task {
  let! existing = loadMicroblogAssignmentsForDocument itemDocumentType itemId ctx
  for e in existing do
    do! Database.deleteDocument ctx e.Id
}

let deleteMicroblogFromItem itemDocumentType itemId microblogId ctx = task {
  let! existing = (Database.getDocumentById microblogId ctx |> Util.taskMap (Option.map JObjectToMicroblogAssignment))

  do! match existing with
      | Some m -> Database.deleteDocument ctx m.Id
      | None -> System.Threading.Tasks.Task.FromResult(())
}

type PostBody = {
  Text: string
}

let postBodyFromContext (ctx: HttpContext) =
  Util.requestBodyFromContextAsJobject ctx
  |> Util.taskResultBind (fun obj ->
    match JObj.getter<string> obj "text" with
    | Some t -> Ok { Text = t }
    | None -> Error "Invalid post body; missing text element"
  )

let addHandler_post itemDocumentType itemId : HttpHandler =
  fun next ctx -> task {
    let! item = Database.getDocumentByTypeAndId itemDocumentType itemId ctx

    return!
      match item with
      | None -> (setStatusCode 404 >=> text $"Unable to find %s{itemDocumentType} with id %s{itemId}") next ctx
      | Some _ -> task {
        let! model =
          postBodyFromContext ctx
          |> Util.taskResultBind (fun m ->
              if System.String.IsNullOrWhiteSpace(m.Text) then
                Error "Text is required"
              else
                Ok m
            )

        return!
          match model with
          | Error msg -> (setStatusCode 400 >=> text msg) next ctx
          | Ok model -> task {
                let doc = {
                  Id = Util.newGuid () |> string
                  ItemDocumentType = itemDocumentType
                  ItemId = itemId
                  Text = model.Text
                  DateAdded = DateTimeOffset.UtcNow
                }
                let doc = doc |> microblogAssignmentToJObject
                let! id = Database.insertDocument ctx doc
                return! (setStatusCode 200 >=> text id) next ctx
          }
      }
  }

let microblogs_get itemDocumentType itemId : HttpHandler =
  fun next ctx -> task {
    let! blogs = loadMicroblogsForDocument itemDocumentType itemId ctx

    let blogs = blogs |> List.map microblogToJObject

    return! json blogs next ctx
  }

let microblogs_delete itemDocumentType itemId blogId : HttpHandler =
  fun next ctx -> task {
    do! deleteMicroblogFromItem itemDocumentType itemId blogId ctx
    return! setStatusCode 200 next ctx
  }

let private editView (microblog: MicroblogAssignment) =
  let pageTitle = "Edit Microblog"
  Items.layout pageTitle Map.empty [
    div [ _class "page-title" ] [ encodedText pageTitle ]
    form [ _name "form"; _method "post" ] [
      table [ ] [
        Items.makeInputRow "Timestamp" (encodedText (microblog.DateAdded.ToString("g")))
        Items.makeTextAreaInputRow "Text" "text" (Some microblog.Text)

        tr [] [
          td [] []
          td [] [ input [ _type "submit"; _value "Save" ] ]
        ]
      ]
    ]
  ]

let microblogs_edit_get id : HttpHandler =
  fun next ctx -> task {
    let! existing =
      Database.getDocumentByTypeAndId documentType id ctx
      |> Util.taskOptionMap JObjectToMicroblogAssignment

    return! match existing with
            | None -> (setStatusCode 404 >=> text "Microblog not found") next ctx
            | Some existing -> htmlView (editView existing) next ctx

  }

let microblogs_edit_post id : HttpHandler =
  fun next ctx -> task {
    let! existing =
      Database.getDocumentByTypeAndId documentType id ctx
      |> Util.taskOptionMap JObjectToMicroblogAssignment

    return! match existing with
            | None -> (setStatusCode 404 >=> text "Microblog not found") next ctx
            | Some existing ->
                let newText = Util.getFormString ctx "text"
                match newText with
                | None -> (setStatusCode 400 >=> text "Invalid request; missing text") next ctx
                | Some newText ->
                    let updatedRecord = { existing with Text = newText }
                    let updatedRecord = updatedRecord |> microblogAssignmentToJObject
                    task {
                      do! Database.upsertDocument ctx updatedRecord
                      return! (redirectTo false $"/admin/microblog/%s{id}") next ctx
                    }

  }