module JObj
open Newtonsoft.Json
open Newtonsoft.Json.Linq

let typeName<'a> = 
    let t = typedefof<'a>
    t.FullName

let fieldExists (obj: JObject) (key: string) =
    if not (obj.ContainsKey(key)) then 
        false
    else
        let token = obj.[key]
        not (token.Type = JTokenType.Null || token.Type = JTokenType.Undefined)

let getter<'a> (obj: JObject) (key: string) =
    if not (fieldExists obj key) then
        None
    else
        match typeName<'a> with
        | typeName when typeName = "System.DateTimeOffset" ->
            try
                let d = obj.Value<string>(key)
                let d = match box d with
                        | null -> (false, System.DateTimeOffset.MinValue)
                        | d -> System.DateTimeOffset.TryParse(string d)
                match d with
                | true, d -> Some ((box d) :?> 'a)
                | false, _ -> None
            with
            | _ -> None
        | _ -> 
            try
                let v = obj.Value<'a>(key)
                match box v with
                | null -> None
                | w -> Some v
            with
            | _ -> None

let setValue<'a> (key: string) (v: 'a) (obj: JObject) =
    let tokenValue =
        match v.GetType() with
        | t when t = typedefof<string> -> JToken.op_Implicit((box v) :?> string)
        | t when t = typedefof<System.DateTimeOffset> -> 
            let stringValue = ((box v) :?> System.DateTimeOffset).ToString("o")
            JToken.op_Implicit(stringValue)
        | t when t = typedefof<bool> -> JToken.op_Implicit((box v) :?> bool)
        | _ -> failwith $"Could not convert field %s{key} to a JToken"
    obj.[key] <- tokenValue
    obj

let setOptionalValue<'a> (key: string) (v: 'a option) (obj: JObject) =
    match v with 
    | None -> obj
    | Some v -> setValue key v obj