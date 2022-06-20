module Util
open Microsoft.AspNetCore.Http

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
        Some (ctx.Request.Form.[key].ToString())
    else
        None


/// Returns an optional map with the form data contained in the context for the given keys
/// If any key is missing from the form data, the returned value will be None
let safeMapBuilder (getter: string -> string option) (keys: string seq) = 
    let rec f (keys: string list) (acc: Option<Map<string, string>>) =
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
                    f rest (Some (acc |> Map.add key v))

    f (Seq.toList keys) (Some Map.empty)

let getFormDataStrings (ctx: HttpContext) (keys: string seq) = 
    safeMapBuilder (getFormString ctx) keys

let getJObjectStrings (obj: Newtonsoft.Json.Linq.JObject) (keys: string seq) =
    safeMapBuilder (JObj.getter<string> obj) keys

let getMapStrings (m: Map<string, string>) (keys: string seq) = 
    safeMapBuilder (fun k -> Map.tryFind k m) keys

let listPrepend (a: 'a list) (b: 'a list) = List.append b a

let newGuid() = System.Guid.NewGuid()

let guidFromString (s: string) = 
    match System.Guid.TryParse(s) with
    | true, g -> Some g
    | false, _ -> None

let dateTimeOffsetFromString (s: string) = 
    match System.DateTimeOffset.TryParse(s) with
    | true, d -> Some d
    | false, _ -> None

