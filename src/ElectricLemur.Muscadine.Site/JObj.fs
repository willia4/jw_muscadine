module JObj
open Newtonsoft.Json
open Newtonsoft.Json.Linq

let getter<'a> (key: string) (obj: JObject) =
    try
        let v = obj.Value<'a>(key)
        match box v with
        | null -> None
        | w -> Some v
    with
    | _ -> None

let requiredGetter<'a when 'a: null> (obj: JObject) (key: string) =
    match getter<'a> key obj with
    | Some v -> v
    | None -> raise (System.InvalidOperationException($"Required key %s{key} not present in JObject"))

let stringValue (key: string) (obj: JObject) = getter<string> key obj
let requiredStringValue (key: string) (obj: JObject) = requiredGetter<string> obj key

let dateValue key obj =
    let d = getter<string> key obj
    match d with
    | Some d -> 
        match System.DateTimeOffset.TryParse(d) with
        | true, d -> Some  d
        | _ -> None
    | None -> None

let requiredDateValue key obj = 
    match dateValue key obj with
    | Some d -> d
    | None -> raise (System.InvalidOperationException($"Required key %s{key} not present in JObject"))