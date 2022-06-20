module JObj
open Newtonsoft.Json
open Newtonsoft.Json.Linq

let getter<'a when 'a: null> (obj: JObject) (key: string)  =
    try
        match obj.Value<'a>(key) with
        | null -> None
        | v -> Some v
    with
    | _ -> None

let requiredGetter<'a when 'a: null> (obj: JObject) (key: string) =
    match getter<'a> obj key with
    | Some v -> v
    | None -> raise (System.InvalidOperationException($"Required key %s{key} not present in JObject"))

let stringValue (key: string) (obj: JObject) = getter<System.String> obj key
let requiredStringValue (key: string) (obj: JObject) = requiredGetter<System.String> obj key
