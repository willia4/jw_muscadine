module Image

open Microsoft.AspNetCore.Http
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Formats
open Util

type ImagePaths = {
    Original: string
    Size1024: string
    Size512: string
    Size256: string
    Size128: string
    Size64: string
}

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
        let id = documentId.ToLowerInvariant().Replace("-", "")
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
        | Error msg -> taskResult (Error msg)
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