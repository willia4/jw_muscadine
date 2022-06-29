module OptionalFields

open Microsoft.AspNetCore.Http
open Newtonsoft.Json.Linq
open ElectricLemur.Muscadine.Site

type OptionalFieldDescriptor<'m, 'a> = {
    Key: string
    Label: string
    getValueFromModel: 'm -> 'a option
    getValueFromContext: HttpContext -> 'a option
    getValueFromJObject: JObject -> 'a option
}

let key (field: OptionalFieldDescriptor<_, _>) = field.Key

let label (field: OptionalFieldDescriptor<_, _>) = field.Label

let modelGetter (field: OptionalFieldDescriptor<'m, 'a>) = field.getValueFromModel

let formGetter (field: OptionalFieldDescriptor<_, 'a>) = field.getValueFromContext

let jobjGetter (field: OptionalFieldDescriptor<_, 'a>) = field.getValueFromJObject

let setJObject g (field: OptionalFieldDescriptor<'m, 'a>) (obj: JObject) = 
    let value = g |> modelGetter field
    match value with
    | None -> obj
    | Some v -> JObj.setValue (key field) v obj
