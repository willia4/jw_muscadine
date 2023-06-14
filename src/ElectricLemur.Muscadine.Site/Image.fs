module ElectricLemur.Muscadine.Site.Image

open System
open System.Collections.Immutable
open FileInfo
open Microsoft.AspNetCore.Http
open Newtonsoft.Json.Linq
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Formats
open Util
open Giraffe
open Giraffe.ViewEngine
open ElectricLemur.Muscadine.Site
open ImagePaths


type ImageLibraryRecord = {
    Id: string
    DateAdded: DateTimeOffset
    Name: string
    ImagePaths: ImagePaths
}

module ImageLibraryRecord =
    let documentType = "imageLibraryRecord"
    //
    // module Fields =
    //     open 
    //     let _id = FormFields.FormField.RequiredStringField ({
    //         Key = "_id"
    //         Label = "Id"
    //         getValueFromModel = (fun b -> Some b.Id)
    //         getValueFromContext = (fun ctx -> None)
    //         getValueFromJObject = (fun obj -> JObj.getter<string> obj "_id")
    //         isUnique = true})
    //
    //     let _dateAdded = FormFields.FormField.RequiredDateTimeField ({
    //         Key = "_dateAdded"
    //         Label = "Date Added"
    //         getValueFromModel = (fun b -> Some b.DateAdded)
    //         getValueFromContext = (fun ctx -> None)
    //         getValueFromJObject = (fun obj -> JObj.getter<System.DateTimeOffset> obj "_dateAdded")
    //         isUnique = false})

    let fromJObject (obj: JObject) =
        let id = JObj.getter<string> obj Database.idField
        let dateAdded = JObj.getter<DateTimeOffset> obj Database.dateAddedField
        let name = JObj.getter<string> obj "name"
        let paths = JObj.getter<ImagePaths> obj "imagePaths"
        
        match id, dateAdded, name, paths with
        | Some id, Some dateAdded, Some name, Some paths ->
            Some {
                    Id = id
                    DateAdded = dateAdded
                    Name = name
                    ImagePaths = paths }
        | _ -> None
        
    let toJObject (record: ImageLibraryRecord) =
        JObj.ofSeq [ (Database.idField, record.Id); (Database.documentTypeField, documentType) ]
        |> JObj.setValue Database.dateAddedField record.DateAdded
        |> JObj.setValue "name" record.Name
        |> JObj.setValue "imagePaths" record.ImagePaths

type Icon =
    | FontAwesome of string
    | Image of ImagePaths

let rec xmlElementFromIcon icon sizeChooser ctx =
    match icon with
    | FontAwesome iconClass -> i [ _class iconClass ] []
    | Image imagePath ->
        let makeUrl = (Util.flip Util.makeUrl) ctx
        imagePath
        |> sizeChooser
        |> Some
        |> Util.addRootPath "/images"
        |> Option.map makeUrl
        |> Option.map (fun path -> img [ _src (path.ToString()) ])
        |> Option.defaultValue (xmlElementFromIcon (FontAwesome "fa-solid fa-cloud-exclamation") sizeChooser ctx)


let private loadImageFromBytes (img: ImmutableArray<byte>) = 
    try
        let mutable format: IImageFormat = null
        let img = Image.Load(img.AsSpan(), &format)
        Ok (img, format)
    with
    | ex -> Error (ex.Message)

let private resizeImage size (original: ImmutableArray<byte>) = task {
    
    match loadImageFromBytes original with
    | Error msg -> return Error msg
    | Ok (img, format) ->
    
        if size > 0 then
            let newWidth = if img.Width >= img.Height then size else 0
            let newHeight = if img.Height >= img.Width then size else 0

            img.Mutate(fun x -> x.Resize(newWidth, newHeight, KnownResamplers.Lanczos3) |> ignore)

        let outputStream = new System.IO.MemoryStream(img.PixelType.BitsPerPixel * img.Width * img.Height)
    
        do! img.SaveAsync(outputStream, format)

        return Ok (outputStream.ToArray().ToImmutableArray())
}

let saveImageToContainer (originalFile: FileInfo) (containerDirectory: string) ctx = task {
    let fullPathFromRelativePath relativePath = joinPath (dataPath ctx) relativePath
    
    let resizeAndSaveImage size (relativePathGetter: 'a -> string) (prev: Result<'a * ImmutableArray<byte>, string>) = task {
        match prev with
        | Error msg -> return (Error msg)
        | Ok (path, (originalBytes: ImmutableArray<byte>)) -> 
            let fullPathToNew = fullPathFromRelativePath (relativePathGetter path)

            let! resizedImage = resizeImage size originalBytes

            match resizedImage with
            | Ok bytes -> 
                saveFileBytes fullPathToNew bytes |> ignore
            
                return (Ok (path, originalBytes))
            | Error msg -> return (Error msg)
    }
    
    let ext = match extensionForFile originalFile with
              | Some ext -> Ok ext
              | None -> Error $"File %s{fileName originalFile} must have an extension"
              
    let paths =
        ext
        |> Result.map (fun ext -> {
            Original = joinPath containerDirectory $"size_original%s{ext}"
            Size1024 = joinPath containerDirectory $"size_1024%s{ext}"
            Size512 = joinPath containerDirectory $"size_512%s{ext}"
            Size256 = joinPath containerDirectory $"size_256%s{ext}"
            Size128 = joinPath containerDirectory $"size_128%s{ext}"
            Size64 = joinPath containerDirectory $"size_64%s{ext}"
        })
        
    let! r = 
        match paths with
        | Error msg -> Task.fromResult (Error msg)
        | Ok paths -> task {
            let! bytes = getBytes originalFile
            return Ok (paths, bytes)
        }

    let! r = r |> resizeAndSaveImage    0 (fun p -> p.Original)
    let! r = r |> resizeAndSaveImage 1024 (fun p -> p.Size1024)
    let! r = r |> resizeAndSaveImage  512 (fun p -> p.Size512)
    let! r = r |> resizeAndSaveImage  256 (fun p -> p.Size256)
    let! r = r |> resizeAndSaveImage  128 (fun p -> p.Size128)
    let! r = r |> resizeAndSaveImage   64 (fun p -> p.Size64)

    return r |> Result.map (fun r -> fst r)
}
let saveImageToDataStore (originalFile: FileInfo) documentType (documentId: string) fileKey ctx = task {
    let containerDirectory = 
        let id = Id.compressId documentId
        joinPath3 documentType fileKey id

    return! saveImageToContainer originalFile containerDirectory ctx
}

let deleteAllImages coverImage ctx =
    deleteRelativePathIfExists coverImage.Original ctx
    deleteRelativePathIfExists coverImage.Size1024 ctx
    deleteRelativePathIfExists coverImage.Size512 ctx
    deleteRelativePathIfExists coverImage.Size256 ctx
    deleteRelativePathIfExists coverImage.Size128 ctx
    deleteRelativePathIfExists coverImage.Size64 ctx
    Task.fromResult ()

let saveImageToLibrary (originalImage: FileInfo) (imageName: string option) ctx = task {
    let id = Util.newGuid () |> string
    let containerDirectory = joinPath "imageLibrary" (Id.compressId id)
    
    let! paths = saveImageToContainer originalImage containerDirectory ctx
    
    return!
        match paths with
        | Ok paths -> task {
            let record = {
                    Id = id
                    DateAdded = DateTimeOffset.UtcNow
                    Name = imageName |> Option.defaultValue (FileInfo.fileName originalImage)
                    ImagePaths = paths 
                }
            do! Database.upsertDocument ctx (ImageLibraryRecord.toJObject record)
            return Ok record
            }
        | Error err -> Error err |> Task.fromResult
}

let loadImageFromLibrary id ctx = task {
    let! record = Database.getDocumentByTypeAndId ImageLibraryRecord.documentType id ctx 
    let record = record |> Option.bind ImageLibraryRecord.fromJObject
    
    return match record with
           | Some record -> Ok record
           | None -> Error $"Image with id %s{id} could not be found"
}

module Handlers =
    open Microsoft.Extensions.Configuration
    let GET_imageRouter (paths: string seq) =
        fun next (ctx: HttpContext) -> task {
            let components = paths |> Seq.skip 1 |> Seq.toArray

            if components.Length <> 4 then
                return! setStatusCode 401 next ctx
            else
                let documentType = components.[0]
                let imageKey = components.[1]
                let idOrSlug = components.[2]
                let fileName = components.[3]
                
                let! id = 
                    if not (Id.isId idOrSlug) then task {
                        let! id = Database.getIdForDocumentTypeAndSlug documentType idOrSlug ctx
                        return id |> Option.map Id.compressId
                    }
                    else 
                        Some (idOrSlug |> Id.compressId) |> Task.fromResult

                match id with
                | Some id ->
                    let path = $"{documentType}/{imageKey}/{id}/{fileName}"
                    let path = path.Replace("/", System.IO.Path.DirectorySeparatorChar.ToString())
                    let path = System.IO.Path.Join((Util.dataPath ctx), path)
                    let file = FileInfo.ofPath path
                    
                    match FileInfo.isValid file with
                    | true ->
                        let config = ctx.GetService<IConfiguration>()
                        let cacheEnabled = config.GetValue<bool>("webOptimizer:enableCaching", false)
                        if cacheEnabled then
                            let cacheAge = System.TimeSpan.FromDays(30).TotalSeconds |> int
                            ctx.Response.Headers.Append("Cache-Control", $"max-age=%d{cacheAge}, public")

                        let setContentType next ctx =
                            match Util.contentTypeForFileInfo file with
                            | Some contentType ->
                                setContentType contentType next ctx
                            | None -> next ctx
                            
                        let fileInfo = FileInfo.ofPath path
                        use! fileStream = FileInfo.toStream fileInfo
                        return! (setContentType >=> streamData false fileStream None None) next ctx
                    | false -> return! setStatusCode 401 next ctx
                | None -> return! setStatusCode 401 next ctx
        }

