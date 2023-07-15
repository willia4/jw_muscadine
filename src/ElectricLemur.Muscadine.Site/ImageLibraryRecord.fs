module ElectricLemur.Muscadine.Site.ImageLibraryRecord
open System
open System.Threading.Tasks
open ElectricLemur.Muscadine.Site.ImagePaths
open ElectricLemur.Muscadine.Site.Image
open Microsoft.AspNetCore.Http
open Newtonsoft.Json.Linq

let documentType = imageLibraryRecordDocumentType
    
let fromJObject = imageLibraryRecordFromJObject
let toJObject = imageLibraryRecordToJObject

let fileExtension = imageLibraryRecordFileExtension

let getImagePaths record =
    let fileExtension = fileExtension record 
    {
        Original = $"images\\%s{Id.compressId record.Id}\\size/original%s{fileExtension}"
        Size1024 = $"images\\%s{Id.compressId record.Id}\\size/1024%s{fileExtension}"
        Size512 = $"images\\%s{Id.compressId record.Id}\\size/512%s{fileExtension}"
        Size256 = $"images\\%s{Id.compressId record.Id}\\size/256%s{fileExtension}"
        Size128 = $"images\\%s{Id.compressId record.Id}\\size/128%s{fileExtension}" 
        Size64 = $"images\\%s{Id.compressId record.Id}\\size/64%s{fileExtension}" 
    }

let toFileInfo = imageLibraryRecordToFileInfo
    
module Fields =
    let _id = FormFields.FormField.RequiredStringField ({
        Key = "_id"
        Label = "Id"
        getValueFromModel = (fun i -> Some i.Id)
        getValueFromContext = (fun ctx -> None)
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "_id")
        isUnique = true})
    
    let _dateAdded = FormFields.FormField.RequiredDateTimeField ({
        Key = "_dateAdded"
        Label = "Date Added"
        getValueFromModel = (fun i -> Some i.DateAdded)
        getValueFromContext = (fun ctx -> None)
        getValueFromJObject = (fun obj -> JObj.getter<System.DateTimeOffset> obj "_dateAdded")
        isUnique = false})
    
    let name = FormFields.FormField.RequiredStringField ({
        Key = "name"
        Label = "Name"
        getValueFromModel = (fun i -> Some i.Name)
        getValueFromContext = (fun ctx -> HttpFormFields.fromContext ctx |> HttpFormFields.stringOptionValue "name")
        getValueFromJObject = (fun obj -> JObj.getter<string> obj "name")
        isUnique = false})
    
    // let contentType = FormFields.FormField.RequiredStringField ({
    //     Key = "contentType"
    //     Label = "Content Type"
    //     getValueFromModel = (fun i -> Some i.ContentType)
    //     getValueFromContext = (fun ctx -> HttpFormFields.fromContext ctx |> HttpFormFields.stringOptionValue "contentType")
    //     getValueFromJObject = (fun obj -> JObj.getter<string> obj "contentType")
    //     isUnique = false})
    
    let imageData = FormFields.FormField.RequiredImagePathsField({
        Key = "imageData"
        Label = "Image"
        getValueFromModel = getImagePaths >> Some
        getValueFromContext = (fun _ -> raise (new NotImplementedException("Cannot get imageData from form fields")))
        getValueFromJObject = fromJObject >> (Option.map getImagePaths)
        isUnique = false})
    
    let allFields = [ _id; _dateAdded; name; imageData ]

let makeAndValidateModelFromContext (existing: ImageLibraryRecord option) (ctx: HttpContext): Task<Result<ImageLibraryRecord, string>> =
    let id = existing |> Option.map (fun i -> i.Id) |> Option.defaultValue (string (Util.newGuid ()))
    let dateAdded = existing |> Option.map (fun i -> i.DateAdded) |> Option.defaultValue System.DateTimeOffset.UtcNow
    let existingContentType = existing |> Option.map (fun i -> i.ContentType) |> Option.defaultValue "application/octet-stream"
    
    match FormFields.validateFieldsOnContext ctx documentType id Fields.allFields with
    | Ok _ ->
        let getFormStringValue f = FormFields.ContextValue.string f ctx |> Option.get

        let uploadedFile =
            ctx
            |> Util.uploadedFiles
            |> List.filter (fun f -> f.Name = FormFields.key Fields.imageData)
            |> List.tryHead
            |> Option.map FileInfo.ofHttpFormFile
            
        let contentTypeTask =
            match uploadedFile with
            | None -> Task.fromResult existingContentType
            | Some f -> Image.detectContentTypeForFileInfo f
            
        contentTypeTask
        |> Task.map (fun contentType ->
                        {
                            Id = id
                            DateAdded = dateAdded
                            Name = getFormStringValue Fields.name
                            ContentType = contentType
                        })
        |> Task.bind (FormFields.validateFieldsOnModel ctx documentType id Fields.allFields)

    | Error msg -> Task.fromResult (Error msg)