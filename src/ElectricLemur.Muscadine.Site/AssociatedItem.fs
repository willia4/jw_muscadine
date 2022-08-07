module AssociatedItem
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
  |> Task.mapSeq jObjToItem

let loadAssociatedItemMapForDocuments documentType itemDocumentType itemIds
    (jObjToItem: (JObject -> 'a)) (associatedIdFromItem: 'a -> string) (resultFromItem: 'a -> 'c) ctx = task {

  let! items = loadAssociatedItemsForDocuments documentType itemDocumentType itemIds jObjToItem ctx
  let items = items |> List.ofSeq
  let mapItemIdToAssociatedItem = Map.ofGroupedList associatedIdFromItem resultFromItem items

  return Map.withPlaceholderIds itemIds [] mapItemIdToAssociatedItem
}

let loadAssociatedRawItemForDocument (documentType: string) (itemDocumentType: string) (itemId: string) ctx =
  loadAssociatedRawItemsForDocuments documentType itemDocumentType [ itemId ] ctx
  |> Task.map Seq.tryHead

let loadAssociatedItemForDocument (documentType: string) (itemDocumentType: string) (itemId: string) (jObjToItem: (JObject -> 'a)) ctx =
  loadAssociatedRawItemForDocument documentType itemDocumentType itemId ctx
  |> Task.map (fun x -> Option.map jObjToItem x)