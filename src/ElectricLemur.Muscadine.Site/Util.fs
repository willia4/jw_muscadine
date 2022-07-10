module Util
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Giraffe

let flip f a b = f b a
let flip3 f a b c = f c b a

let mapResultToOption f r = 
    match r with
    | Ok v -> Some (f v)
    | Error _ -> None

let appendSeqToList (a: 'a list) (b: 'a seq) =
    let rec m acc remaining =
        if (remaining |> Seq.isEmpty) then
            acc
        else
            m ((Seq.head remaining) :: a) (Seq.tail remaining)

    m (List.rev a) (b |> Seq.rev) |> List.rev

let unwrapSeqOfOptions(s: seq<option<'a>>) =
    if Seq.forall Option.isSome s then
        Some (Seq.map Option.get s)
    else
        None

let unwrapListOfOptions(s: List<option<'a>>) =
    if List.forall Option.isSome s then
        Some (List.map Option.get s)
    else
        None

let mergeMaps (merger: 'v -> 'v -> 'v) (m1: Map<'k, 'v>) (m2: Map<'k, 'v>) =
    m2
    |> Map.fold (fun res k v ->
        res 
        |> Map.change k (fun otherV -> 
            match otherV with
            | Some otherV -> Some (merger v otherV)
            | None -> Some v)
      ) m1
    
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

let joinPath (a: string) b = System.IO.Path.Join(a, b)
let joinPath3 (a: string) b c = System.IO.Path.Join(a, b, c)

let formFileToBytes (formFile: IFormFile) = task {
    use m = new System.IO.MemoryStream()
    do! formFile.CopyToAsync(m)
    return m.ToArray()
}

let saveFile (filePath: string) (file: byte array) = task {
    let dir = System.IO.Path.GetDirectoryName(filePath)
    if not (System.IO.Directory.Exists(dir)) then
        System.IO.Directory.CreateDirectory(dir) |> ignore

    try
        use ms = new System.IO.MemoryStream(file)

        use write = System.IO.File.OpenWrite(filePath)
        do! ms.CopyToAsync(write)

        return Ok filePath
    with
    | ex -> return Error (ex.Message)
}
let saveFormFile (filePath: string) (file: IFormFile) = task {
    let! bytes = formFileToBytes file
    return! saveFile filePath bytes
}

let extensionForFormFile (f: IFormFile) =
    let ext = System.IO.Path.GetExtension(f.FileName)
    if (System.String.IsNullOrWhiteSpace(ext)) then 
        None
    else
        Some ext

let saveFileToDataStore (f: IFormFile) documentType (documentId: string) fileKey ctx = task {
    let relativePath = 
        match extensionForFormFile f with
        | None -> Error $"File %s{f.FileName} must have an extension"
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

let listPrepend (a: 'a list) (b: 'a list) = List.append b a
let seqPrepend (a: 'a seq) (b: 'a seq) = Seq.append b a

let appendToListIf p item l =
    if p then
        List.append l [ item ]
    else
        l

let appendToSeqIf p item s =
    if p then
        Seq.append s [ item ]
    else
        s

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

let requestBodyFromContextAsJobject ctx = task {
    let! body = requestBodyFromContext ctx
  return (JObj.parseString body)
}

let taskResult v = System.Threading.Tasks.Task.FromResult(v)

let taskMap (f: 'a -> 'b) (t: System.Threading.Tasks.Task<'a>) = task {
    let! r = t
    return (f r)
}

let taskSeqMap (f: 'a -> 'b) (t: System.Threading.Tasks.Task<'a seq>) = task {
    let! r = t
    return (r |> Seq.map f)
}

let taskListMap (f: 'a -> 'b) (t: System.Threading.Tasks.Task<'a list>) = task {
    let! r = t
    return (r |> List.map f)
}

let taskResultBind (f: 'a -> Result<'b, 'e>) (t: System.Threading.Tasks.Task<Result<'a, 'e>>) = task {
    let! r = t
    return Result.bind f r
}

let taskResultMap (f: 'a -> 'b) (t: System.Threading.Tasks.Task<Result<'a, 'e>>) =
    t |> taskResultBind (fun x -> Ok (f x))

let taskResultMapError (f: 'e -> 'f) (t: System.Threading.Tasks.Task<Result<'a, 'e>>) = task {
    let! r = t
    return Result.mapError f r
}

let taskOptionMap (f: 'a -> 'b) (t: System.Threading.Tasks.Task<Option<'a>>) = task {
    let! r = t
    return
        match r with
        | Some v -> Some (f v)
        | None -> None
}

let emptyDiv = Giraffe.ViewEngine.HtmlElements.div [ Giraffe.ViewEngine.Attributes._style "display: none"] []