module ElectricLemur.Muscadine.Site.AssociatedItem
open ElectricLemur.Muscadine.Site
open Newtonsoft.Json.Linq

let itemDocumentTypeField = "itemDocumentType"
let itemIdField = "itemId"

let loadAssociatedRawItemsForDocuments (documentType: string) (itemDocumentType: string) (itemIds: string seq) ctx =
  let filter =
    Database.Filters.empty
    |> Database.Filters.addEquals Database.documentTypeField documentType
    |> Database.Filters.addEquals itemDocumentTypeField itemDocumentType

  Database.getDocumentsById itemIdField itemIds filter ctx

let loadAssociatedItemsForDocuments (documentType: string) (itemDocumentType: string) (itemIds: string seq) (jObjToItem: (JObject -> 'a)) ctx =
  loadAssociatedRawItemsForDocuments documentType itemDocumentType itemIds ctx
  |> Task.map (Seq.map jObjToItem)

let loadAssociatedItemMapForDocuments documentType itemDocumentType itemIds
    (jObjToItem: (JObject -> 'a)) (associatedIdFromItem: 'a -> string) (resultFromItem: 'a -> 'c) ctx =
    loadAssociatedItemsForDocuments documentType itemDocumentType itemIds jObjToItem ctx
    |> Task.map List.ofSeq
    |> Task.map (Map.ofGroupedList associatedIdFromItem resultFromItem)
    |> Task.map (Map.withPlaceholderIds itemIds [])

let loadAssociatedRawItemForDocument (documentType: string) (itemDocumentType: string) (itemId: string) ctx =
  loadAssociatedRawItemsForDocuments documentType itemDocumentType [ itemId ] ctx
  |> Task.map Seq.tryHead

let loadAssociatedItemForDocument (documentType: string) (itemDocumentType: string) (itemId: string) (jObjToItem: (JObject -> 'a)) ctx =
  loadAssociatedRawItemForDocument documentType itemDocumentType itemId ctx
  |> Task.map (fun x -> Option.map jObjToItem x)