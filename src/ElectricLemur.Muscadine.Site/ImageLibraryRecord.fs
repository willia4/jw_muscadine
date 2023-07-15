module ElectricLemur.Muscadine.Site.ImageLibraryRecord
open System
open ElectricLemur.Muscadine.Site.ImagePaths
open Newtonsoft.Json.Linq

type ImageLibraryRecord = {
    Id: string
    DateAdded: DateTimeOffset
    Name: string
    ContentType: string
}

let documentType = "imageLibraryRecord"

let fromJObject (obj: JObject) =
    let id = JObj.getter<string> obj Database.idField
    let documentType' = JObj.getter<string> obj Database.documentTypeField
    let dateAdded = JObj.getter<DateTimeOffset> obj Database.dateAddedField
    let name = JObj.getter<string> obj "name"
    let contentType = JObj.getter<string> obj "contentType"
    
    match id, documentType', dateAdded, name, contentType with
    | Some id, Some documentType', Some dateAdded, Some name, Some contentType when documentType' = documentType ->
        Some {
                Id = id
                DateAdded = dateAdded
                Name = name
                ContentType = contentType }
    | _ -> None
    
let toJObject (record: ImageLibraryRecord) =
    JObj.ofSeq [ (Database.idField, record.Id); (Database.documentTypeField, documentType) ]
    |> JObj.setValue Database.dateAddedField record.DateAdded
    |> JObj.setValue "name" record.Name
    |> JObj.setValue "contentType" record.ContentType

let fileExtension (record: ImageLibraryRecord) = Util.fileExtensionForContentType record.ContentType

let getImagePaths record =
    let fileExtension = fileExtension record 
    {
        Original = $"images\\%s{record.Id}\\size_original%s{fileExtension}"
        Size1024 = $"images\\%s{record.Id}\\size_1024%s{fileExtension}"
        Size512 = $"images\\%s{record.Id}\\size_512%s{fileExtension}"
        Size256 = $"images\\%s{record.Id}\\size_256%s{fileExtension}"
        Size128 = $"images\\%s{record.Id}\\size_128%s{fileExtension}" 
        Size64 = $"images\\%s{record.Id}\\size_64%s{fileExtension}" 
    }
    
let toFileInfo record ctx =
    let fileExtension = fileExtension record 
    let path = $"images/%s{record.Id}/size_original%s{fileExtension}"
    let path = System.IO.Path.Join((Util.dataPath ctx), path)
    FileInfo.ofPath path