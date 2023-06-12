module Util
open System.Collections.Immutable
open FileInfo
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Configuration
open Giraffe
open Giraffe.ViewEngine
open Microsoft.Extensions.Caching.Memory
open CommunityToolkit.HighPerformance

let flip f a b = f b a
let flip3 f a b c = f c b a

let getFormString (ctx: HttpContext) (key: string) =
    if ctx.Request.Form.ContainsKey(key) then
        Some (string ctx.Request.Form.[key])
    else
        None

let addRootPathSeparator (p: string option) = 
    match p with
    | None -> None
    | Some p -> 
        if p.StartsWith("/") then 
            Some p 
        else 
            Some $"/%s{p}"

let addRootPath (root: string) (p: string option) =
    let root = addRootPathSeparator (Some root) |> Option.get

    match addRootPathSeparator p with
    | None -> None
    | Some p -> 
        if p.StartsWith(root) then
            Some p
        else
            Some $"%s{root}%s{p}"

let uploadedFiles (ctx: HttpContext) =
    seq {
        for f in ctx.Request.Form.Files do
        yield f
    } |> Seq.toList

let dataPath (ctx: HttpContext) = ctx.GetService<IConfiguration>().GetValue<string>("DataDirectory")
let baseUrl (ctx: HttpContext) =
    let b = ctx.GetService<IConfiguration>().GetValue<string>("BaseUrl")
    if b.EndsWith("/") then b else $"%s{b}/"

let makeUrl (path: string) ctx =
    let path =
        if path.StartsWith("/") then
            path.Substring(1)
        else path
    let path = path.Replace("\\", "/")
    let b = baseUrl ctx
    (new System.Uri($"%s{b}%s{path}"))

let joinPath (a: string) b = System.IO.Path.Join(a, b)
let joinPath3 (a: string) b c = System.IO.Path.Join(a, b, c)

let joinUrlParts (a: string) (b: string) =
    let a = if not (a.EndsWith("/")) then $"{a}/" else a
    let b = if b.StartsWith("/") then b.Remove(0, 1) else b
    let result = $"{a}{b}"
    result

let saveFileBytes (filePath: string) (fileBytes: ImmutableArray<byte>) = task {
    let dir = System.IO.Path.GetDirectoryName(filePath)
    if not (System.IO.Directory.Exists(dir)) then
        System.IO.Directory.CreateDirectory(dir) |> ignore

    try
        use bufferStream = fileBytes.AsMemory().AsStream()

        use write = System.IO.File.OpenWrite(filePath)
        do! bufferStream.CopyToAsync(write)

        return Ok filePath
    with
    | ex -> return Error (ex.Message)
}
let saveFormFile (filePath: string) (file: FileInfo) = task {
    let! bytes = getBytes file
    return! saveFileBytes filePath bytes
}

let extensionForFile (f: FileInfo) =
    let ext = fileExtension f
    if (System.String.IsNullOrWhiteSpace(ext)) then 
        None
    else
        Some ext

let saveFileToDataStore (f: FileInfo) documentType (documentId: string) fileKey ctx = task {
    let relativePath = 
        match extensionForFile f with
        | None -> Error $"File %s{fileName f} must have an extension"
        | Some ext -> 
            let id = Id.compressId documentId
            Ok (joinPath3 documentType fileKey $"%s{id}%s{ext}")
            

    match relativePath with
    | Error msg -> return Error msg
    | Ok relativePath -> 
        let fullPath = joinPath (dataPath ctx) relativePath
        let! saveResult = saveFormFile fullPath f

        return match saveResult with
               | Ok _ -> Ok (Some (relativePath.Replace("\\", "/")))
               | Error msg -> Error msg
}

let directoryIsEmpty (fullPath: string) =
    if System.IO.Directory.Exists(fullPath) then
        let fileCount = System.IO.Directory.GetFiles(fullPath).Length
        let dirCount = System.IO.Directory.GetDirectories(fullPath).Length

        (fileCount + dirCount) = 0
    else
        false

let deleteRelativePathIfExists (relativePath: string) ctx =
    try
        let realPath = joinPath (dataPath ctx) relativePath
        if System.IO.File.Exists(realPath) then
            System.IO.File.Delete(realPath)

        let realPath = System.IO.Path.GetDirectoryName(realPath)
        if directoryIsEmpty realPath then
            System.IO.Directory.Delete(realPath)
    with
    | _ -> ()

/// Returns an optional map with the form data contained in the context for the given keys
/// If any required key is missing from the form data, the returned value will be None
/// If any optional key is missing from the form data, the map element for that key will be None
let safeMapBuilder (getter: string -> string option) (requiredKeys: string seq) (optionalKeys: string seq) =
    let rec requiredKeysMap (keys: string list) (acc: Option<Map<string, string option>>) =
        match keys with 
        | [] -> acc
        | key::rest ->
            match acc with
            | None -> None
            | Some acc -> 
                let data = getter key
                match data with
                | None -> None
                | Some v ->
                    requiredKeysMap rest (Some (acc |> Map.add key (Some v)))

    let m = requiredKeysMap (Seq.toList requiredKeys) (Some Map.empty)

    let rec optionalKeysMap (keys: string list) (acc: Option<Map<string, string option>>) =
        match keys with
        | [] -> acc
        | key::rest ->
            match acc with
            | None -> None
            | Some acc ->
                let data = getter key
                optionalKeysMap rest (Some (acc |> Map.add key data))
    
    optionalKeysMap (Seq.toList optionalKeys) m

let getFormDataStrings (ctx: HttpContext) (requiredKeys: string seq) (optionalKeys: string seq) = 
    safeMapBuilder (getFormString ctx) requiredKeys optionalKeys

let getJObjectStrings (obj: Newtonsoft.Json.Linq.JObject) (requiredKeys: string seq) (optionalKeys: string seq) =
    safeMapBuilder (JObj.getter<string> obj) requiredKeys optionalKeys

let getMapStrings (m: Map<string, string option>) (requiredKeys: string seq) (optionalKeys: string seq) = 
    safeMapBuilder (fun k -> Map.tryFind k m |> Option.flatten) requiredKeys optionalKeys

let contentTypeForFileName (fileName: string) =
    match fileName.ToLowerInvariant() with
    | f when f.EndsWith(".ico") -> Some "image/vnd.microsoft.icon"
    | f when f.EndsWith(".jpg") -> Some "image/jpeg"
    | f when f.EndsWith(".png") -> Some "image/png"
    | f when f.EndsWith(".css") -> Some "text/css; charset=UTF-8"
    | f when f.EndsWith(".scss") -> Some "text/css; charset=UTF-8"
    | f when f.EndsWith(".js") -> Some "application/javascript; charset=UTF-8"
    | _ -> None

let newGuid () = System.Guid.NewGuid()

let guidFromString (s: string) = 
    match System.Guid.TryParse(s) with
    | true, g -> Some g
    | false, _ -> None

let dateTimeOffsetFromString (s: string) = 
    match System.DateTimeOffset.TryParse(s) with
    | true, d -> Some d
    | false, _ -> None

let boolFromString (s: string) =
    match s with
    | "on" -> Some true
    | _ -> 
        match System.Boolean.TryParse(s) with
        | true, v -> Some v
        | false, _ -> None

let requestBodyFromContext (ctx: HttpContext) = task {
    use reader = new System.IO.StreamReader(ctx.Request.Body)
    return! reader.ReadToEndAsync()
}

let requestBodyFromContextAsJobject ctx = ctx |> requestBodyFromContext |> Task.map (JObj.parseString)

let emptyDiv = div [ _style "display: none"] []

let hashFile fullPath =
    use sha256 = System.Security.Cryptography.SHA256.Create()
    use stream = System.IO.File.OpenRead(fullPath)
    let bytes = sha256.ComputeHash(stream)
    Microsoft.AspNetCore.WebUtilities.WebEncoders.Base64UrlEncode(bytes)

let private fileVersion assetDirectory file (ctx: HttpContext) =
    let env = ctx.GetService<IWebHostEnvironment>()
    let config = ctx.GetService<IConfiguration>()
    let cacheEnabled = config.GetValue<bool>("webOptimizer:enableCaching", false)

    if cacheEnabled then
        let cache = ctx.GetService<IMemoryCache>()

        let fullPath = System.IO.Path.Join(env.WebRootPath, assetDirectory, file)
        let cacheKey = $"fileVersion-%s{fullPath}"
        cache.GetOrCreate(cacheKey, fun cacheEntry ->
            cacheEntry.SlidingExpiration <- System.TimeSpan.FromDays(2)
            hashFile fullPath)
    else
        (newGuid() |> string).ToLowerInvariant().Replace("-", "")

let cssLinkTag cssFile ctx=
    let version = fileVersion "css" cssFile ctx
    link [ (_rel "stylesheet"); (_type "text/css"); (_href $"/css/%s{cssFile}?v=%s{version}") ]

let javascriptTag javascriptFile ctx =
    let version = fileVersion "js" javascriptFile ctx
    script [ _src $"/js/%s{javascriptFile}?v={version}" ] []

let extractEmbeddedResource name =
    let assembly = System.Reflection.Assembly.GetExecutingAssembly()

    let name = $"ElectricLemur.Muscadine.Site.%s{name}"
    use stream = assembly.GetManifestResourceStream(name)

    use output = new System.IO.MemoryStream()
    stream.CopyTo(output)

    output.ToArray()

let extractEmbeddedTextFile name =
    System.Text.Encoding.UTF8.GetString(extractEmbeddedResource name)

let textToParagraphs (text: string) =
    let lines = text.Split("\n");
    let paragraphs = lines |> Seq.chunkByPredicate System.String.IsNullOrWhiteSpace

    paragraphs
    |> Seq.map (fun p ->
        let lines = System.String.Join("\n", p)
        Giraffe.ViewEngine.HtmlElements.p [] [ encodedText lines ]
    )
    |> Seq.toList

let makeId prefix =
    let id = System.Guid.NewGuid().ToString().Replace("-","").ToLowerInvariant()
    $"{prefix}-{id}"