module Util
open Microsoft.AspNetCore.Http

let flip f a b = f b a
let flip3 f a b c = f c b a

let appendSeqToList (a: 'a list) (b: 'a seq) =
    let rec m acc remaining =
        if (remaining |> Seq.isEmpty) then
            acc
        else
            m ((Seq.head remaining) :: a) (Seq.tail remaining)

    m (List.rev a) (b |> Seq.rev) |> List.rev

let unwrapListOfOptions(s: List<option<'a>>) =
    if List.forall Option.isSome s then
        Some (List.map Option.get s)
    else
        None

let getFormString (ctx: HttpContext) (key: string) =
    if ctx.Request.Form.ContainsKey(key) then
        Some (string ctx.Request.Form.[key])
    else
        None


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
    safeMapBuilder (flip JObj.getter<string> obj) requiredKeys optionalKeys

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