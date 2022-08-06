module AssociatedItem
open ElectricLemur.Muscadine.Site
open Newtonsoft.Json.Linq

let itemDocumentTypeField = "itemDocumentType"
let itemIdField = "itemId"

let loadAssociatedItemsForDocuments (documentType: string) (itemDocumentType: string) (itemIds: string seq) (jObjToItem: (JObject -> 'a)) ctx = task {
  let filter =
    Database.Filters.empty
    |> Database.Filters.addEquals Database.documentTypeField documentType
    |> Database.Filters.addEquals itemDocumentTypeField itemDocumentType

  let! documents = Database.getDocumentsById itemIdField itemIds filter ctx
  return (documents |> Seq.map jObjToItem)
}


let loadAssociatedItemMapForDocuments documentType itemDocumentType itemIds
    (jObjToItem: (JObject -> 'a)) (associatedIdFromItem: 'a -> string) (resultFromItem: 'a -> 'c) ctx = task {

  let! items = loadAssociatedItemsForDocuments documentType itemDocumentType itemIds jObjToItem ctx
  let items = items |> List.ofSeq
  let mapItemIdToAssociatedItem = Map.ofGroupedList associatedIdFromItem resultFromItem items

  return Map.withPlaceholderIds itemIds [] mapItemIdToAssociatedItem
}