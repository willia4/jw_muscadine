module RequiredFields

open Microsoft.AspNetCore.Http
open Newtonsoft.Json.Linq
open ElectricLemur.Muscadine.Site

type RequiredFieldDescriptor<'m, 'a> = {
    Key: string
    Label: string
    getValueFromModel: 'm -> 'a
    getValueFromContext: HttpContext -> 'a option
    getValueFromJObject: JObject -> 'a
}

let key (field: RequiredFieldDescriptor<_, _>) = field.Key

let label (field: RequiredFieldDescriptor<_, _>) = field.Label

let modelGetter (field: RequiredFieldDescriptor<'m, 'a>) = field.getValueFromModel

let formGetter (field: RequiredFieldDescriptor<_, 'a>) = field.getValueFromContext

let jobjGetter (field: RequiredFieldDescriptor<_, 'a>) = field.getValueFromJObject

let requiredStringValidator field = Items.requiredStringValidator (field |> key) (field |> modelGetter)
let stringFieldUniquenessValidator ctx documentType id field = Items.uniqueStringFieldValidator ctx documentType id (key field) ((modelGetter field) >> Some)

let setJObject g (field: RequiredFieldDescriptor<'m, 'a>) (obj: JObject) = 
    JObj.setValue (key field) (g|> modelGetter field) obj
