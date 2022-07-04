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

let private tagAssignmentToJObject itemDocumentType a = 
    new Newtonsoft.Json.Linq.JObject()
    |> JObj.setValue Database.idField a.Id
    |> JObj.setValue Database.documentTypeField documentType
    |> JObj.setValue "itemDocumentType" itemDocumentType
    |> JObj.setValue "itemId" a.ItemId
    |> JObj.setValue "tag" a.Tag

let private JObjectToTagAssignment obj = 
    let getter k =JObj.getter<string> obj k |> Option.get

    {
        Id = getter Database.idField
        ItemId = getter "itemId"
        ItemDocumentType = getter "itemDocumentType"
        Tag = getter "tag"
    }

let private loadTagAssignmentsForDocuments (itemDocumentType: string) ids ctx = task {
    let loadTagsForChunk (ids: string seq) = task {
        let filter = 
            ids 
            |> Seq.fold (fun filter id -> filter |> Database.Filters.addIn "itemId" id) Database.Filters.empty
        
        let filter = 
            filter 
            |> Database.Filters.addEquals Database.documentTypeField documentType
            |> Database.Filters.addEquals "itemDocumentType" itemDocumentType

        let! tagAssignments = Database.getDocumentsForFilter filter ctx

        return tagAssignments 
            |> Seq.map JObjectToTagAssignment
    }

    let mutable loadedAssignments = List.empty
    // Mongo docs say to only do "tens" of items for "in" queries, so let's set a reasonable limit of 30
    let chunks = ids |> Seq.chunkBySize 30
    for chunk in chunks do
        let! tags = loadTagsForChunk chunk
        loadedAssignments <- List.append loadedAssignments (tags |> Seq.toList)
    
    return loadedAssignments
}

let private loadTagAssignmentsForDocument itemDocumentType id ctx = task {
    return! loadTagAssignmentsForDocuments itemDocumentType [id] ctx
}

let loadTagsForDocuments itemDocumentType ids ctx = task {
    let! assignments = loadTagAssignmentsForDocuments itemDocumentType ids ctx

    let mapItemIdToTags = 
        assignments 
        |> List.fold (fun m a -> 
            m |> Map.change a.ItemId (fun tags -> 
                match tags with
                | Some tags -> Some (List.append tags [ a.Tag ] |> List.distinct)
                | None -> Some [ a.Tag ]
            )) Map.empty

    return ids
        |> Seq.fold (fun m id -> 
            match m |> Map.containsKey id with 
            | true -> m
            | false -> Map.add id [] m
        ) mapItemIdToTags

}

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
        let! _ = Database.insertDocument ctx (tagAssignmentToJObject itemDocumentType i)
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