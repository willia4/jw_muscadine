module Image

open Microsoft.AspNetCore.Http
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Formats
open Util
open Giraffe
open Giraffe.ViewEngine
open ElectricLemur.Muscadine.Site

type ImagePaths = {
    Original: string
    Size1024: string
    Size512: string
    Size256: string
    Size128: string
    Size64: string
}

let choose64 i = i.Size64
let choose128 i = i.Size128
let choose256 i = i.Size256
let choose512 i = i.Size512
let choose1024 i = i.Size1024
let chooseOriginal i = i.Original

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


let private loadImageFromBytes (img: byte array) = 
    try
        let mutable format: IImageFormat = null
        let img = Image.Load(img, &format)
        Ok (img, format)
    with
    | ex -> Error (ex.Message)

let private resizeImage size (original: byte array) = task {
    
    match loadImageFromBytes original with
    | Error msg -> return Error msg
    | Ok (img, format) ->
    
        if size > 0 then
            let newWidth = if img.Width >= img.Height then size else 0
            let newHeight = if img.Height >= img.Width then size else 0

            img.Mutate(fun x -> x.Resize(newWidth, newHeight, KnownResamplers.Lanczos3) |> ignore)

        let outputStream = new System.IO.MemoryStream(img.PixelType.BitsPerPixel * img.Width * img.Height)
    
        do! img.SaveAsync(outputStream, format)

        return Ok (outputStream.ToArray())
}

let saveImageToDataStore (originalFile: IFormFile) documentType (documentId: string) fileKey ctx = task {
    let containerDirectory = 
        let id = Id.compressId documentId
        joinPath3 documentType fileKey id

    let fullPathFromRelativePath relativePath = joinPath (dataPath ctx) relativePath

    // prev: A Result that determines if the function does anything at all. Wraps a tuple. 
    // The snd item in the tuple is the buffer for the original image. 
    // The fst item in the tuple is an object which can be passed to relativePathGetter to retrieve the path to save the image to
    let resizeAndSaveImage size relativePathGetter prev = task {
        match prev with
        | Error msg -> return (Error msg)
        | Ok (path, (originalBytes: byte array)) -> 
            let fullPathToNew = fullPathFromRelativePath (relativePathGetter path)

            let! resizedImage = resizeImage size originalBytes

            match resizedImage with
            | Ok bytes -> 
                saveFile fullPathToNew bytes |> ignore
            
                return (Ok (path, originalBytes))
            | Error msg -> return (Error msg)
    }
        
    let ext = match extensionForFormFile originalFile with
              | Some ext -> Ok ext
              | None -> Error $"File %s{originalFile.FileName} must have an extension"

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
            let! bytes = formFileToBytes originalFile
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

let deleteAllImages coverImage ctx =
    deleteRelativePathIfExists coverImage.Original ctx
    deleteRelativePathIfExists coverImage.Size1024 ctx
    deleteRelativePathIfExists coverImage.Size512 ctx
    deleteRelativePathIfExists coverImage.Size256 ctx
    deleteRelativePathIfExists coverImage.Size128 ctx
    deleteRelativePathIfExists coverImage.Size64 ctx
    Task.fromResult ()

module Handlers =
    open Microsoft.Extensions.Configuration
    let GET_imageRouter (paths: string seq) =
        fun next (ctx: HttpContext) -> task {
            let fullPath = paths |> Seq.head
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

                    match System.IO.File.Exists(path) with
                    | true ->
                        let config = ctx.GetService<IConfiguration>()
                        let cacheEnabled = config.GetValue<bool>("webOptimizer:enableCaching", false)
                        if cacheEnabled then
                            let cacheAge = System.TimeSpan.FromDays(30).TotalSeconds |> int
                            ctx.Response.Headers.Append("Cache-Control", $"max-age=%d{cacheAge}, public")

                        return! streamFile false path None None next ctx
                    | false -> return! setStatusCode 401 next ctx
                | None -> return! setStatusCode 401 next ctx
        }

