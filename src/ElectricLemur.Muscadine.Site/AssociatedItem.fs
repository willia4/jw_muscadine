module AssociatedItem
open ElectricLemur.Muscadine.Site
open Newtonsoft.Json.Linq

let itemDocumentTypeField = "itemDocumentType"
let itemIdField = "itemId"

let loadAssociatedItemsForDocuments (documentType: string) (itemDocumentType: string) (itemIds: string seq) (jObjToItem: (JObject -> 'a)) ctx = task {
  let loadChunk (ids: string seq) = task {
    let filter =
      ids
      |> Seq.fold (fun filter id -> filter |> Database.Filters.addIn itemIdField id)
                  Database.Filters.empty
    let filter =
      filter
      |> Database.Filters.addEquals Database.documentTypeField documentType
      |> Database.Filters.addEquals itemDocumentTypeField itemDocumentType

    let! associatedItems = Database.getDocumentsForFilter filter Database.NoLimit ctx
    return associatedItems |> Seq.map jObjToItem
  }

  // Mongo docs say to only do "tens" of items for "in" queries, so let's set a reasonable limit of 30
  let mutable loadedItems = List.empty
  let chunks = itemIds |> Seq.chunkBySize 30
  for chunk in chunks do
      let! items = loadChunk chunk
      loadedItems <- List.append loadedItems (items |> Seq.toList)

  return loadedItems
}


let loadAssociatedItemMapForDocuments documentType itemDocumentType itemIds
    (jObjToItem: (JObject -> 'a)) (associatedIdFromItem: 'a -> string) (resultFromItem: 'a -> 'c) ctx = task {

  let! items = loadAssociatedItemsForDocuments documentType itemDocumentType itemIds jObjToItem ctx
  let mapItemIdToAssociatedItem = Map.ofGroupedList associatedIdFromItem resultFromItem items

  return Map.withPlaceholderIds itemIds [] mapItemIdToAssociatedItem
}