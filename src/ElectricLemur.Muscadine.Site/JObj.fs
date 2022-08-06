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
                let item = obj.[key]
                match item.Type with
                | t when t = JTokenType.Object -> 
                    Some (item.ToObject<'a>())
                    
                | _ -> 
                    let v = item.Value<'a>()
                    match box v with
                    | null -> None
                    | _ -> Some v
            with
            | _ -> None

let setValue<'a> (key: string) (v: 'a) (obj: JObject) =
    let tokenValue =
        match v.GetType() with
        | t when t = typedefof<string> -> JToken.op_Implicit((box v) :?> string)
        | t when t = typedefof<System.DateTimeOffset> -> 
            let stringValue = ((box v) :?> System.DateTimeOffset).ToOffset(System.TimeSpan.Zero).ToString("o")
            JToken.op_Implicit(stringValue)
        | t when t = typedefof<bool> -> JToken.op_Implicit((box v) :?> bool)
        | t when not t.IsPrimitive -> JToken.FromObject(v)
        | _ -> failwith $"Could not convert field %s{key} to a JToken"
    obj.[key] <- tokenValue
    obj

let setOptionalValue<'a> (key: string) (v: 'a option) (obj: JObject) =
    match v with 
    | None -> obj
    | Some v -> setValue key v obj

let parseString (s: string) =
    try
        Ok (JObject.Parse(s))
    with
    | ex -> Error ex.Message