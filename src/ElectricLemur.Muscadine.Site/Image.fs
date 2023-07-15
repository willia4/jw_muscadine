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
    ContentType: string
}

let imageLibraryRecordDocumentType = "imageLibraryRecord"

let imageLibraryRecordFromJObject (obj: JObject) =
    let id = JObj.getter<string> obj Database.idField
    let documentType' = JObj.getter<string> obj Database.documentTypeField
    let dateAdded = JObj.getter<DateTimeOffset> obj Database.dateAddedField
    let name = JObj.getter<string> obj "name"
    let contentType = JObj.getter<string> obj "contentType"
    
    match id, documentType', dateAdded, name, contentType with
    | Some id, Some documentType', Some dateAdded, Some name, Some contentType when documentType' = imageLibraryRecordDocumentType ->
        Some {
                Id = id
                DateAdded = dateAdded
                Name = name
                ContentType = contentType }
    | _ -> None
    
let imageLibraryRecordToJObject (record: ImageLibraryRecord) =
    JObj.ofSeq [ (Database.idField, record.Id); (Database.documentTypeField, imageLibraryRecordDocumentType) ]
    |> JObj.setValue Database.dateAddedField record.DateAdded
    |> JObj.setValue "name" record.Name
    |> JObj.setValue "contentType" record.ContentType
    
let imageLibraryRecordFileExtension (record: ImageLibraryRecord) = Util.fileExtensionForContentType record.ContentType

let imageLibraryRecordToFileInfo (record: ImageLibraryRecord) ctx =
    let fileExtension = imageLibraryRecordFileExtension record 
    let path = $"imageLibraryRecord/imageData/%s{Id.compressId record.Id}/size_original%s{fileExtension}"
    let path = System.IO.Path.Join((Util.dataPath ctx), path)
    FileInfo.ofPath path
    
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

let private loadImageFromFileInfo (img: FileInfo) = task {
    let! bytes = FileInfo.getBytes img
    return loadImageFromBytes bytes
}
    
let imageFormatFromContentType contentType =
    match contentType with
    | c when c = "image/jpeg" -> SixLabors.ImageSharp.Formats.Jpeg.JpegFormat.Instance :> IImageFormat
    | c when c = "image/jpg" -> SixLabors.ImageSharp.Formats.Jpeg.JpegFormat.Instance
    | c when c = "image/gif" -> SixLabors.ImageSharp.Formats.Gif.GifFormat.Instance
    | c when c = "image/png" -> SixLabors.ImageSharp.Formats.Png.PngFormat.Instance
    | c when c = "image/bmp" -> SixLabors.ImageSharp.Formats.Bmp.BmpFormat.Instance
    | _ -> SixLabors.ImageSharp.Formats.Jpeg.JpegFormat.Instance

let imageFormatFromFileName fileName = Util.contentTypeForFileName fileName |> Option.defaultValue "" |> imageFormatFromContentType

let detectContentTypeForFileInfo img = task {
    let defaultContentType = "application/octet-stream"
    let! loadedResult = loadImageFromFileInfo img
    return
        match loadedResult with
        | Ok (_, format) ->
            match format with
            | :? SixLabors.ImageSharp.Formats.Jpeg.JpegFormat -> "image/jpeg"
            | :? SixLabors.ImageSharp.Formats.Gif.GifFormat -> "image/gif"
            | :? SixLabors.ImageSharp.Formats.Png.PngFormat -> "image/png"
            | :? SixLabors.ImageSharp.Formats.Bmp.BmpFormat -> "image/bmp"
            | _ -> defaultContentType
        | Error _ -> defaultContentType
}

let private resizeImage size outputFormat (original: ImmutableArray<byte>) = task {
    
    match loadImageFromBytes original with
    | Error msg -> return Error msg
    | Ok (img, inputFormat) ->
        let outputFormat = outputFormat |> Option.defaultValue inputFormat
        if outputFormat = inputFormat && size = 0 then
            return Ok original // don't actually have to do any work if we want the original size and format
        else
            let size = Math.Min(Math.Abs(size), 2000) // cap size at something reasonable 
            
            if size > 0 then
                let newWidth = if img.Width >= img.Height then size else 0
                let newHeight = if img.Height >= img.Width then size else 0

                img.Mutate(fun x -> x.Resize(newWidth, newHeight, KnownResamplers.Lanczos3) |> ignore)

            let outputStream = new System.IO.MemoryStream(img.PixelType.BitsPerPixel * img.Width * img.Height)
        
            do! img.SaveAsync(outputStream, outputFormat)

            return Ok (outputStream.ToArray().ToImmutableArray())
}

let private createConvertedAndResizedFileInfo size outputFormat (original: FileInfo) =
    let fileExtension = FileInfo.fileExtension original
    let originalFormat =
        fileExtension
        |> Util.contentTypeForFileExtension
        |> Option.map imageFormatFromContentType
        
    let requestedFormatIsOriginal =
        match originalFormat, outputFormat with
        | _, None -> true
        | None, _ -> false
        | Some originalFormat, Some outputFormat -> originalFormat = outputFormat

    if requestedFormatIsOriginal && size = 0 then
        original        
    else
        let provider = fun () -> task {
            let! bytes = FileInfo.getBytes original
            let! resizeResult = resizeImage size outputFormat bytes
            match resizeResult with
            | Error _ -> return ImmutableArray<byte>.Empty
            | Ok newBytes -> return newBytes 
        }
        let extension =
            match outputFormat with
            | Some outputFormat -> outputFormat.FileExtensions |> Seq.tryHead |> Option.defaultValue (FileInfo.fileExtension original)
            | _ -> (FileInfo.fileExtension original)
        let extension = Util.addDotToFileExtension extension
        
        FileInfo.ofBytesProvider $"size_size%s{extension}" provider

let saveImageToContainer (originalFile: FileInfo) (containerDirectory: string) ctx = task {
    let fullPathFromRelativePath relativePath = joinPath (dataPath ctx) relativePath
    
    let resizeAndSaveImage size (relativePathGetter: 'a -> string) (prev: Result<'a * ImmutableArray<byte>, string>) = task {
        match prev with
        | Error msg -> return (Error msg)
        | Ok (path, (originalBytes: ImmutableArray<byte>)) -> 
            let fullPathToNew = fullPathFromRelativePath (relativePathGetter path)

            let! resizedImage = resizeImage size None originalBytes

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
                    ContentType = Util.contentTypeForFileInfo originalImage |> Option.defaultValue "application/octet-stream"
                }
            do! Database.upsertDocument ctx (imageLibraryRecordToJObject record)
            return Ok record
            }
        | Error err -> Error err |> Task.fromResult
}

let loadImageFromLibrary id ctx = task {
    let! record = Database.getDocumentByTypeAndId imageLibraryRecordDocumentType id ctx 
    let record = record |> Option.bind imageLibraryRecordFromJObject
    
    return match record with
           | Some record -> Ok record
           | None -> Error $"Image with id %s{id} could not be found"
}

module Handlers =
    open Microsoft.Extensions.Configuration
    // /images/book/coverImage/65cffde48d5346b580420a7ba00f455e/size_256.gif
    let oldRegex = System.Text.RegularExpressions.Regex("/images/(.*?)/(.*?)/(.*?)/(.*)")
    let newRegex = System.Text.RegularExpressions.Regex("/images/([a-zA-Z0-9].+?)/size/(.+?)\\.(.+?)$")
    type ImageRouteType =
        | OldImageRoute of string array
        | NewImageRoute of string*string*string
        | ErrorRoute
        
    let getImageRoute (ctx: HttpContext) =
        let path = string ctx.Request.Path
        match (newRegex.Match(path)), (oldRegex.Match(path)) with
        | (newMatch, _) when newMatch.Success -> NewImageRoute (newMatch.Groups[1].Value, newMatch.Groups[2].Value, newMatch.Groups[3].Value)
        | (_, oldMatch) when oldMatch.Success -> OldImageRoute [|oldMatch.Groups[1].Value; oldMatch.Groups[2].Value; oldMatch.Groups[3].Value; oldMatch.Groups[4].Value|]
        | _ -> ErrorRoute

    let GET_imageRouter next (ctx: HttpContext)  = task {
            
            match getImageRoute ctx with
            | OldImageRoute components -> 
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
                        | false -> return! setStatusCode 404 next ctx
                    | None -> return! setStatusCode 404 next ctx
            | NewImageRoute (id, sizePart, extension) ->
                let! record =
                    id
                    |> Id.expandId
                    |> Option.map (fun id -> Database.getDocumentByTypeAndId imageLibraryRecordDocumentType id ctx)
                    |> Option.defaultValue (Task.fromResult None)
                    |> Task.map (function
                                 | Some obj -> imageLibraryRecordFromJObject obj
                                 | None -> None)
                
                let requestedExtension = Util.addDotToFileExtension extension

                let originalFile = record |> Option.map (fun record -> imageLibraryRecordToFileInfo record ctx)
                let outputContentType = requestedExtension |> Util.contentTypeForFileExtension
                let outputFormat =  outputContentType |> Option.map imageFormatFromContentType
                 
                let isOriginalContentType =
                    match outputContentType, record with
                    | None, _ -> true
                    | Some ct, Some record when ct = record.ContentType -> true
                    | _ -> false
                    
                let possibleCachedImage =
                    match isOriginalContentType, record with
                    | true, Some record ->
                        let possiblePath = $"imageLibraryRecord/imageData/%s{Id.compressId record.Id}/size_%s{sizePart}%s{requestedExtension}"
                        let possiblePath = System.IO.Path.Join((Util.dataPath ctx), possiblePath)
                        let image = FileInfo.ofPath possiblePath
                        if FileInfo.isValid image then Some image else None 
                    | _, _ -> None

                let outputImage =
                    match possibleCachedImage, originalFile with
                    | Some image, _ -> Some image
                    | None, Some originalFile ->
                        match FileInfo.isValid originalFile with
                        | true ->
                            let newSize =
                                match sizePart with
                                | s when s = "original" -> 0
                                | s ->
                                    match Int32.TryParse(s) with
                                    | true, i -> i
                                    | _ -> 0
                            Some (createConvertedAndResizedFileInfo newSize outputFormat originalFile)
                        | false -> None
                    | None, None -> None

                match outputImage with
                | Some outputImage ->
                    use! fileStream = FileInfo.toStream outputImage
                    let setContentType next ctx =
                        match Util.contentTypeForFileInfo outputImage with
                        | Some contentType ->
                            setContentType contentType next ctx
                        | None -> next ctx
                        
                    let setCache next (ctx: HttpContext) =
                        let config = ctx.GetService<IConfiguration>()
                        let cacheEnabled = config.GetValue<bool>("webOptimizer:enableCaching", false)

                        if cacheEnabled then
                            let cacheAge = TimeSpan.FromDays(2).TotalSeconds |> int
                        
                            ctx.Response.Headers.Append(
                                "Cache-Control", $"max-age=%d{cacheAge}, public")

                        next ctx
                        
                    return! (setCache >=> setContentType >=> streamData false fileStream None None) next ctx
                | None -> return! setStatusCode 404 next ctx
            | _ ->
                return! setStatusCode 404 next ctx
        }

