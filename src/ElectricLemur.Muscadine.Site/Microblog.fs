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

let private loadMicroblogAssignmentsForDocuments itemDocumentType ids ctx =
  AssociatedItem.loadAssociatedItemsForDocuments documentType itemDocumentType ids JObjectToMicroblogAssignment ctx

let private loadMicroblogAssignmentsForDocument itemDocumentType id ctx =
  loadMicroblogAssignmentsForDocuments itemDocumentType [ id ] ctx

let loadMicroblogsForDocuments itemDocumentType ids ctx = task {
  let! r = AssociatedItem.loadAssociatedItemMapForDocuments documentType itemDocumentType ids JObjectToMicroblogAssignment (fun i -> i.ItemId) MicroblogAssignmentToMicroblog ctx

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
