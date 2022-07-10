module Tag
open ElectricLemur.Muscadine.Site
open Microsoft.AspNetCore.Http
open Giraffe
open Microsoft.Extensions.Caching.Memory

type TaggedItem<'a> = {
    Item: 'a
    Tags: string list
}

let documentType = "tagged-item"
let formKey = "tags"

type private TagAssignment = {
    Id: string
    ItemId: string
    ItemDocumentType: string
    Tag: string
}

let private tagAssignmentToJObject a =
    new Newtonsoft.Json.Linq.JObject()
    |> JObj.setValue Database.idField a.Id
    |> JObj.setValue Database.documentTypeField documentType
    |> JObj.setValue AssociatedItem.itemDocumentTypeField a.ItemDocumentType
    |> JObj.setValue AssociatedItem.itemIdField a.ItemId
    |> JObj.setValue "tag" a.Tag

let private JObjectToTagAssignment obj =
    let getter k =JObj.getter<string> obj k |> Option.get

    {
        Id = getter Database.idField
        ItemId = getter AssociatedItem.itemIdField
        ItemDocumentType = getter AssociatedItem.itemDocumentTypeField
        Tag = getter "tag"
    }

let private loadTagAssignmentsForDocuments (itemDocumentType: string) ids ctx =
    AssociatedItem.loadAssociatedItemsForDocuments documentType itemDocumentType ids JObjectToTagAssignment ctx

let private loadTagAssignmentsForDocument itemDocumentType id ctx =
    loadTagAssignmentsForDocuments itemDocumentType [id] ctx

let loadTagsForDocuments itemDocumentType ids ctx =
    AssociatedItem.loadAssociatedItemMapForDocuments documentType itemDocumentType ids JObjectToTagAssignment (fun i -> i.Id) (fun i -> i.Tag)ctx

let loadTagsForDocument itemDocumentType itemId ctx = task {
    let! tags = loadTagsForDocuments itemDocumentType [itemId] ctx
    return tags
            |> Map.tryFind itemId
            |> function
                | Some tags -> tags
                | None -> []
}

let setTagsForDocument itemDocumentType itemId (tags: string seq) ctx = task {
    let! existing = loadTagAssignmentsForDocument itemDocumentType itemId ctx
    let toDelete = existing |> List.filter (fun e -> not (Seq.contains e.Tag tags))
    
    let toAdd = tags |> Seq.filter (fun t -> not (List.exists (fun e -> e.Tag = t) existing)) |> Seq.toList
    let toAdd = 
        toAdd
        |> List.map (fun a -> { 
            Id = string (Util.newGuid ());
            ItemId = itemId;
            Tag = a;
            ItemDocumentType = itemDocumentType 
        })
        
    for i in toDelete do
        do! Database.deleteDocument ctx i.Id

    for i in toAdd do
        let! _ = Database.insertDocument ctx (tagAssignmentToJObject i)
        ()

    return ()
}

let clearTagsForDocument itemDocumentType itemId ctx = task {
    let! existing = loadTagAssignmentsForDocument itemDocumentType itemId ctx
    for i in existing do
        do! Database.deleteDocument ctx i.Id
}

let getExistingTags ctx = task {
    let filter = 
        Database.Filters.empty 
        |> Database.Filters.addEquals Database.documentTypeField documentType

    let! tags = Database.getDistinctValues<string> "tag" filter ctx

    return tags |> Seq.toList
}


let getExistingTagsForDocumentType itemDocumentType ctx = task {
    let filter = 
        Database.Filters.empty 
        |> Database.Filters.addEquals Database.documentTypeField documentType
        |> Database.Filters.addEquals "itemDocumentType" itemDocumentType
        
    let! tags = Database.getDistinctValues<string> "tag" filter ctx
    return tags |> Seq.sort |> Seq.toList
}

let saveTagsForForm itemDocumentType itemId key ctx = task {
    let tags = FormFields.stringListValue key (ctx |> FormFields.fromContext)
    do! setTagsForDocument itemDocumentType itemId tags ctx
}