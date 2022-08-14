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

type EnrichedMicroblog = {
  ItemName: string
  ItemIcon: Image.Icon
  ItemDocumentType: string
  ItemId: string
  Item: Newtonsoft.Json.Linq.JObject option
  Microblog: Microblog
  Link: string option
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

let private readItemData itemData =
  let itemName = Items.readNameOrDefault itemData
  let itemIcon = Items.readItemImageOrDefault itemData
  let slug =  Items.tryReadSlug itemData

  itemName, itemIcon, slug

let private enrichMicroblog (microblogAssignment: MicroblogAssignment) itemData ctx =
  let (name, icon, slug) = readItemData itemData
  {
    ItemName = name
    ItemIcon = icon
    ItemDocumentType = microblogAssignment.ItemDocumentType
    ItemId = microblogAssignment.ItemId
    Item = itemData
    Microblog = MicroblogAssignmentToMicroblog microblogAssignment
    Link = slug |> Option.bind (fun slug -> Items.getLinkToItem microblogAssignment.ItemDocumentType slug ctx)
  }

let private loadItemDataForMicroblogAssignment (microblogAssignment: MicroblogAssignment) ctx =
  Database.getDocumentByTypeAndId microblogAssignment.ItemDocumentType microblogAssignment.ItemId ctx
  |> Task.map readItemData


let private loadLinkedItemsData linkedItemIds ctx = task {
  let! linkedItems = Database.getDocumentsById Database.idField linkedItemIds Database.Filters.empty ctx
  let linkedItems =
    linkedItems
    |> Seq.map (fun obj -> (JObj.getter<string> obj Database.idField |> Option.defaultValue ""), obj)
    |> Seq.filter (fun (id, _) -> not (System.String.IsNullOrWhiteSpace(id)))
    |> Map.ofSeq
  return linkedItems
}

let loadRecentMicroblogsForItem (since: System.DateTimeOffset) (itemId: string option) limit ctx = task {
  let filter =
    Database.Filters.empty
    |> Database.Filters.byDocumentType documentType
    |> Database.Filters.addLessThanOrEqualTo "_dateAdded" (since.ToOffset(System.TimeSpan.Zero).ToString("o"))

  let filter =
    match itemId with
    | Some itemId -> filter |> Database.Filters.addEquals AssociatedItem.itemIdField itemId
    | None -> filter

  let sort =
    Database.Sort.empty
    |> Database.Sort.byDescending "_dateAdded"

  let! documents = Database.getDocumentsForFilterAndSort filter sort limit ctx
  let documents =
    documents
    |> Seq.map JObjectToMicroblogAssignment

  let linkedItems = documents |> Seq.map (fun d -> d.ItemId)
  let! linkedItems = loadLinkedItemsData linkedItems ctx

  let documents =
    documents
    |> Seq.map (fun assignment ->
      let linkedItem = linkedItems |> Map.tryFind assignment.ItemId
      enrichMicroblog assignment linkedItem ctx
    )

  return documents
}

let loadRecentMicroblogsForItemType (since: System.DateTimeOffset) (itemDocumentType: string) limit ctx = task {
  let filter =
    Database.Filters.empty
    |> Database.Filters.byDocumentType documentType
    |> Database.Filters.addLessThanOrEqualTo "_dateAdded" (since.ToOffset(System.TimeSpan.Zero).ToString("o"))
    |> Database.Filters.addEquals AssociatedItem.itemDocumentTypeField itemDocumentType

  let sort =
    Database.Sort.empty
    |> Database.Sort.byDescending "_dateAdded"

  let! documents = Database.getDocumentsForFilterAndSort filter sort limit ctx
  let documents =
    documents
    |> Seq.map JObjectToMicroblogAssignment

  let linkedItems = documents |> Seq.map (fun d -> d.ItemId)
  let! linkedItems = loadLinkedItemsData linkedItems ctx

  let documents =
    documents
    |> Seq.map (fun assignment ->
      let linkedItem = linkedItems |> Map.tryFind assignment.ItemId
      enrichMicroblog assignment linkedItem ctx
    )

  return documents
}

let loadRecentMicroblogs (since: System.DateTimeOffset) limit ctx =
  loadRecentMicroblogsForItem since None limit ctx

let loadMostRecentMicroblogForItem itemId ctx =
  loadRecentMicroblogsForItem (System.DateTimeOffset.UtcNow) (Some itemId) (Database.Limit 1) ctx
  |> Task.map (Seq.tryHead)

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
  let! existing = Database.getDocumentById microblogId ctx |> Task.map (Option.map JObjectToMicroblogAssignment)

  do! match existing with
      | Some m -> Database.deleteDocument ctx m.Id
      | None -> Task.fromResult ()
}

type PostBody = {
  Text: string
}

let postBodyFromContext (ctx: HttpContext) =
  Util.requestBodyFromContextAsJobject ctx
  |> Task.map (Result.bind (fun obj ->
    match JObj.getter<string> obj "text" with
    | Some t -> Ok { Text = t }
    | None -> Error "Invalid post body; missing text element"
  ))

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

module Handlers =
  let POST_add itemDocumentType itemId : HttpHandler =
    fun next ctx -> task {
      let! item = Database.getDocumentByTypeAndId itemDocumentType itemId ctx

      return!
        match item with
        | None -> (setStatusCode 404 >=> text $"Unable to find %s{itemDocumentType} with id %s{itemId}") next ctx
        | Some _ -> task {
          let! model =
            postBodyFromContext ctx
            |> Task.map (Result.bind (fun m ->
                if System.String.IsNullOrWhiteSpace(m.Text) then
                  Error "Text is required"
                else
                  Ok m
              ))

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

  let GET_list itemDocumentType itemId : HttpHandler =
    fun next ctx -> task {
      let! blogs = loadMicroblogsForDocument itemDocumentType itemId ctx

      let blogs = blogs |> List.map microblogToJObject

      return! json blogs next ctx
    }

  let DELETE itemDocumentType itemId blogId : HttpHandler =
    fun next ctx -> task {
      do! deleteMicroblogFromItem itemDocumentType itemId blogId ctx
      return! setStatusCode 200 next ctx
    }



  let GET_edit id : HttpHandler =
    fun next ctx -> task {
      let! existing =
        Database.getDocumentByTypeAndId documentType id ctx
        |> Task.map (Option.map JObjectToMicroblogAssignment)

      return! match existing with
              | None -> (setStatusCode 404 >=> text "Microblog not found") next ctx
              | Some existing -> htmlView (editView existing) next ctx

    }

  let POST_edit id : HttpHandler =
    fun next ctx -> task {
      let! existing =
        Database.getDocumentByTypeAndId documentType id ctx
        |> Task.map (Option.map JObjectToMicroblogAssignment)

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

  let GET_atomFeed (documentType: string option) (pluralTopic: string) selfLinkMaker : HttpHandler =
    fun next ctx -> task {
      let since = System.DateTimeOffset.UtcNow

      let! entries =
        match documentType with
        | Some documentType -> loadRecentMicroblogsForItemType since documentType (Database.Limit 100) ctx
        | None -> loadRecentMicroblogs since (Database.Limit 100) ctx

      let entries = entries |> Seq.sortByDescending (fun e -> e.Microblog.DateAdded)
      let dateUpdated = entries |> Seq.tryHead |> Option.map (fun e -> e.Microblog.DateAdded) |> Option.defaultValue (System.DateTimeOffset.UtcNow)

      let currentYear = System.DateTime.Now.Year
      let copyrightDate = if currentYear > 2021 then $"2021 - %d{currentYear}" else "2022"
      let entries =
        entries
        |> Seq.toList
        |> List.map (fun e ->
          tag "entry" [] [
            tag "title" [] [ rawText e.ItemName ]
            tag "id" [] [ rawText $"urn:uuid:%s{e.Microblog.Id}" ]
            tag "published" [] [ rawText (e.Microblog.DateAdded.ToString("o")) ]
            tag "updated" [] [ rawText (e.Microblog.DateAdded.ToString("o")) ]
            tag "summary" [ (attr "type" "html" )] [ encodedText (Markdig.Markdown.ToPlainText(e.Microblog.Text)) ]
            tag "content" [ (attr "type" "xhtml") ] [
              div [ (attr "xmlns" "http://www.w3.org/1999/xhtml") ] [
                yield (h3 [] [ encodedText e.ItemName ])

                // only include actual images in the atom feed
                match e.ItemIcon with
                | Image.Image _ -> yield! [
                    Image.xmlElementFromIcon e.ItemIcon Image.choose256 ctx
                    br []
                  ]
                | _ -> yield! []

                yield rawText (Markdig.Markdown.ToHtml(e.Microblog.Text))
              ]
            ]
            tag "author" [] [
              tag "name" [] [ encodedText "James Williams" ]
            ]
          ]
        )


      let textInfo = (new System.Globalization.CultureInfo("en-US", false)).TextInfo
      let titleCasePluralTopic = textInfo.ToTitleCase(pluralTopic)

      let feed = tag "feed" [ (attr "xmlns" "http://www.w3.org/2005/Atom")] ([
        tag "title" [] [ rawText $"James Williams - Microblog Entries - %s{titleCasePluralTopic}" ]
        tag "subtitle" [] [ rawText $"James Williams' thoughts about {pluralTopic}" ]
        tag "id" [] [ rawText "urn:uuid:cd30bdd1-a3c3-4fde-87fc-8b66b147d1c6" ]
        link [ (_rel "self"); (_href (ctx |> selfLinkMaker |> string)) ]
        tag "icon" [] [ rawText (Util.makeUrl "/img/head_logo_256.png" ctx |> string)]
        tag "rights" [] [ rawText $"© {copyrightDate} James Williams"]
        tag "updated" [] [ rawText (dateUpdated.ToString("o")) ]

      ] |> List.prepend entries)

      let bytes = RenderView.AsBytes.xmlNode feed
      //let s = RenderView.AsString.xmlNode feed

      //return! text s next ctx
      ctx.SetContentType "text/html; charset=utf-8"
      return! (ctx.WriteBytesAsync bytes)
    }