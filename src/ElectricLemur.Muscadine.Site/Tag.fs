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

let private loadTagAssignmentsForDocuments (itemDocumentType: string) itemIds ctx =
    AssociatedItem.loadAssociatedItemsForDocuments documentType itemDocumentType itemIds JObjectToTagAssignment ctx

let private loadTagAssignmentsForDocument itemDocumentType itemId ctx =
    loadTagAssignmentsForDocuments itemDocumentType [itemId] ctx

let loadTagsForDocuments itemDocumentType itemIds ctx =
    AssociatedItem.loadAssociatedItemMapForDocuments documentType itemDocumentType itemIds JObjectToTagAssignment (fun i -> i.ItemId) (fun i -> i.Tag) ctx

let loadTagsForDocument itemDocumentType itemId ctx =
    loadTagsForDocuments itemDocumentType [itemId] ctx
    |> Task.map (fun m -> Map.tryFind itemId m
                          |> function
                             | Some tags -> tags
                             | None -> [])

let setTagsForDocument itemDocumentType itemId (tags: string seq) ctx = task {
    let! existing = loadTagAssignmentsForDocument itemDocumentType itemId ctx
    let toDelete = existing |> Seq.filter (fun e -> not (Seq.contains e.Tag tags))

    let toAdd = tags |> Seq.filter (fun t -> not (Seq.exists (fun e -> e.Tag = t) existing)) |> Seq.toList
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

let clearTagsForDocument itemDocumentType itemId ctx =
    loadTagAssignmentsForDocument itemDocumentType itemId ctx
    |> Task.map (Seq.map (fun tag -> tag.Id))
    |> Task.bind (Seq.iterAsync (Database.deleteDocument ctx))


let getExistingTags ctx =
    let filter =
        Database.Filters.empty
        |> Database.Filters.addEquals Database.documentTypeField documentType

    Database.getDistinctValues<string> "tag" filter ctx
    |> Task.map Seq.sort
    |> Task.map Seq.toList


let getExistingTagsForDocumentType itemDocumentType ctx =
    let filter =
        Database.Filters.empty
        |> Database.Filters.addEquals Database.documentTypeField documentType
        |> Database.Filters.addEquals "itemDocumentType" itemDocumentType

    Database.getDistinctValues<string> "tag" filter ctx
    |> Task.map (Seq.sort)
    |> Task.map (Seq.toList)

let saveTagsForForm itemDocumentType itemId key ctx =
    let tags = HttpFormFields.stringListValue key (ctx |> HttpFormFields.fromContext)
    setTagsForDocument itemDocumentType itemId tags ctx

let private getOrphanedTags ctx =
    let mapper x =
        try
            Some (JObjectToTagAssignment x)
        with
        | _ -> None

    let asyncPredicate tag =
        Database.getDocumentByTypeAndId tag.ItemDocumentType tag.ItemId ctx
        |> Task.map Option.isNone

    Database.getDocumentsByType documentType mapper Database.NoLimit ctx
    |> Task.bind (Seq.filterAsync asyncPredicate)


let orphanedTagsAsJObjects ctx = getOrphanedTags ctx |> Task.map (Seq.map tagAssignmentToJObject)

let deleteOrphanedTags ctx =
    getOrphanedTags ctx
    |> Task.map (Seq.map (fun tag -> tag.Id))
    |> Task.bind (Seq.iterAsync (fun id -> Database.deleteDocument ctx id))

let itemIdsContainingTags itemDocumentType (tags: seq<string>) ctx =
    let filter =
        Database.Filters.empty
        |> Database.Filters.byDocumentType documentType
        |> Database.Filters.addEquals AssociatedItem.itemDocumentTypeField itemDocumentType

    let filter =
        tags |> Seq.fold (fun filter nextTag -> Database.Filters.addIn "tag" nextTag filter) filter

    Database.getDocumentsForFilter filter (Database.NoLimit) ctx
    |> Task.map (Seq.map JObjectToTagAssignment)
    |> Task.map (Seq.map (fun t -> t.ItemId))

let sortTagsForDisplay (tags: seq<string>) =
    let neverShown = [ "in-progress"; "backlog" ]
    let sortFirst = [ "finished"; "did-not-finish" ]
    let sortFirstOrder =
        sortFirst
        |> List.mapi (fun i t -> t, i)
        |> Map.ofList

    let (left, right) =
        tags
        |> Seq.filter (fun t -> neverShown |> List.contains t |> not )
        |> Seq.fold (fun (left, right) t ->
            let goLeft = sortFirst |> List.contains t
            let left = if goLeft then (List.append left [t]) else left
            let right = if (goLeft) then right else (List.append right [t])

            left, right) (List.empty, List.empty)

    let left =
        left
        |> List.sortBy (fun t -> Map.tryFind t sortFirstOrder |> Option.defaultValue (System.Int32.MaxValue))
    let right = right |> List.sort

    List.append left right