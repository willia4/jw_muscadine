module FormFields
open ElectricLemur.Muscadine.Site

type FieldDescriptor<'m, 'f> = {
    Key: string
    Label: string
    getValueFromModel: 'm -> 'f option
    getValueFromContext: Microsoft.AspNetCore.Http.HttpContext -> 'f option
    getValueFromJObject: Newtonsoft.Json.Linq.JObject -> 'f option
}

type FormField<'m> =
  | RequiredStringField of FieldDescriptor<'m, string>
  | RequiredDateTimeField of FieldDescriptor<'m, System.DateTimeOffset>
  | RequiredBooleanField of FieldDescriptor<'m, bool>
  | RequiredImagePathsField of FieldDescriptor<'m, Image.ImagePaths>
  | OptionalStringField of FieldDescriptor<'m, string>
  | OptionalDateTimeField of FieldDescriptor<'m, System.DateTimeOffset>
  | OptionalBooleanField of FieldDescriptor<'m, bool>
  | OptionalImagePathsField of FieldDescriptor<'m, Image.ImagePaths>


let key (field : FormField<_>) =
  match field with
  | RequiredStringField ff -> ff.Key
  | RequiredDateTimeField ff -> ff.Key
  | RequiredBooleanField ff -> ff.Key
  | RequiredImagePathsField ff -> ff.Key
  | OptionalStringField ff -> ff.Key
  | OptionalDateTimeField ff -> ff.Key
  | OptionalBooleanField ff -> ff.Key
  | OptionalImagePathsField ff -> ff.Key

let label (field : FormField<_>) =
  match field with
  | RequiredStringField ff -> ff.Label
  | RequiredDateTimeField ff -> ff.Label
  | RequiredBooleanField ff -> ff.Label
  | RequiredImagePathsField ff -> ff.Label
  | OptionalStringField ff -> ff.Label
  | OptionalDateTimeField ff -> ff.Label
  | OptionalBooleanField ff -> ff.Label
  | OptionalImagePathsField ff -> ff.Label

module ModelValue =
  let string field (model: 'a) =
    match field with
    | RequiredStringField ff -> ff.getValueFromModel model
    | OptionalStringField ff -> ff.getValueFromModel model
    | _ -> None

  let dateTime field (model: 'a) =
    match field with
    | RequiredDateTimeField ff -> ff.getValueFromModel model
    | OptionalDateTimeField ff -> ff.getValueFromModel model
    | _ -> None

  let bool field (model: 'a) =
    match field with
    | RequiredBooleanField ff -> ff.getValueFromModel model
    | OptionalBooleanField ff -> ff.getValueFromModel model
    | _ -> None

  let imagePaths field (model: 'a) =
    match field with
    | RequiredImagePathsField ff -> ff.getValueFromModel model
    | OptionalImagePathsField ff -> ff.getValueFromModel model
    | _ -> None

module ContextValue =
    let string field ctx =
      match field with
      | RequiredStringField ff -> ff.getValueFromContext ctx
      | OptionalStringField ff -> ff.getValueFromContext ctx
      | _ -> None

    let dateTime field ctx =
      match field with
      | RequiredDateTimeField ff -> ff.getValueFromContext ctx
      | OptionalDateTimeField ff -> ff.getValueFromContext ctx
      | _ -> None

module DatabaseValue =
  open Newtonsoft.Json.Linq

  let string field obj =
    match field with
    | RequiredStringField ff -> ff.getValueFromJObject obj
    | OptionalStringField ff -> ff.getValueFromJObject obj
    | _ -> None

  let dateTime field obj =
    match field with
    | RequiredDateTimeField ff -> ff.getValueFromJObject obj
    | OptionalDateTimeField ff -> ff.getValueFromJObject obj
    | _ -> None

  let imagePaths field obj =
    match field with
    | RequiredImagePathsField ff -> ff.getValueFromJObject obj
    | OptionalImagePathsField ff -> ff.getValueFromJObject obj
    | _ -> None

let stringFieldUniquenessValidator ctx documentType id field =
  Items.uniqueStringFieldValidator ctx documentType id (key field) (ModelValue.string field)

let setJObject model field obj =
    match field with
    | RequiredStringField ff -> JObj.setOptionalValue (key field) (ff.getValueFromModel model) obj
    | RequiredDateTimeField ff -> JObj.setOptionalValue (key field) (ff.getValueFromModel model) obj
    | RequiredBooleanField ff -> JObj.setOptionalValue (key field) (ff.getValueFromModel model) obj
    | RequiredImagePathsField ff -> JObj.setOptionalValue (key field) (ff.getValueFromModel model) obj
    | OptionalStringField ff -> JObj.setOptionalValue (key field) (ff.getValueFromModel model) obj
    | OptionalDateTimeField ff -> JObj.setOptionalValue (key field) (ff.getValueFromModel model) obj
    | OptionalBooleanField ff -> JObj.setOptionalValue (key field) (ff.getValueFromModel model) obj
    | OptionalImagePathsField ff -> JObj.setOptionalValue (key field) (ff.getValueFromModel model) obj

let requiredStringValidator ff =
  let stringGetter m =
    match (ModelValue.string ff) m with
    | Some v -> v
    | None -> null

  Items.requiredStringValidator (key ff) stringGetter

module View =
  let modelStringValue ff m = m |> Option.bind (ModelValue.string ff)
  let modelBoolValue ff m = m |> Option.bind (ModelValue.bool ff)
  let modelImagePathsValue ff m = m |> Option.bind (ModelValue.imagePaths ff)

  let makeTextAreaRow ff lineCount m =
    Items.makeTextAreaInputRow (label ff) (key ff) lineCount (modelStringValue ff m)

  let makeTextRow ff m =
    // special case some fields to make them be multi-line text areas
    let key = (key ff)
    if key = "description" then
      makeTextAreaRow ff 5 m
    else
      Items.makeTextInputRow (label ff) key (modelStringValue ff m)

  let makeCheckboxRow ff m =
    Items.makeCheckboxInputRow (label ff) (key ff) (modelBoolValue ff m)

  let makeImageRow ff m =
    let v =
      (modelImagePathsValue ff m)
      |> Option.map (fun paths -> paths.Size512)

    Items.makeImageInputRow (label ff) (key ff) v

  let makeFormFieldRow ff m =
    match ff with
    | RequiredStringField _ -> makeTextRow ff m
    | OptionalStringField _ -> makeTextRow ff m

    | RequiredBooleanField _ -> makeCheckboxRow ff m
    | OptionalBooleanField _ -> makeCheckboxRow ff m

    | RequiredImagePathsField _ -> makeImageRow ff m
    | OptionalImagePathsField _ -> makeImageRow ff m

    | RequiredDateTimeField _ -> failwith "Not Implemented"
    | OptionalDateTimeField _ -> failwith "Not Implemented"


