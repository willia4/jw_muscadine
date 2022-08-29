module FormFields
open ElectricLemur.Muscadine.Site

type FieldDescriptor<'m, 'f> = {
    Key: string
    Label: string
    getValueFromModel: 'm -> 'f option
    getValueFromContext: Microsoft.AspNetCore.Http.HttpContext -> 'f option
    getValueFromJObject: Newtonsoft.Json.Linq.JObject -> 'f option
    isUnique: bool
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

let isRequired field =
  match field with
  | RequiredBooleanField _ -> true
  | RequiredDateTimeField _ -> true
  | RequiredStringField _ -> true
  | RequiredImagePathsField _ -> true
  | OptionalStringField _ -> false
  | OptionalDateTimeField _ -> false
  | OptionalBooleanField _ -> false
  | OptionalImagePathsField _ -> false

let isUnique field =
  match field with
  | RequiredStringField ff -> ff.isUnique
  | RequiredDateTimeField ff -> ff.isUnique
  | RequiredBooleanField ff -> ff.isUnique
  | RequiredImagePathsField ff -> ff.isUnique
  | OptionalStringField ff -> ff.isUnique
  | OptionalDateTimeField ff -> ff.isUnique
  | OptionalBooleanField ff -> ff.isUnique
  | OptionalImagePathsField ff -> ff.isUnique

let viewFieldsForAllFields allFields =
  allFields
  |> Seq.filter(fun f ->
      (key f) <> Database.idField &&
      (key f) <> Database.dateAddedField)
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

let stringFieldUniquenessValidator ctx documentType id field m =
  match field with
  | RequiredStringField _ ->
      Items.uniqueStringFieldValidator ctx documentType id (key field) (ModelValue.string field) m
  | OptionalStringField _ ->
      Items.uniqueStringFieldValidator ctx documentType id (key field) (ModelValue.string field) m
  | _ -> Task.fromResult (Ok m)

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

let validatedRequiredFieldOnModel ff m =
  let k = key ff
  let err = Error $"%s{k} is required and cannot be empty"

  match ff with
  | RequiredStringField _ ->
      let s = ModelValue.string ff m |> Option.defaultValue ""

      if (System.String.IsNullOrWhiteSpace(s)) then
        err
      else
        Ok m

  | RequiredDateTimeField _ ->
      match ModelValue.dateTime ff m with
      | Some _ -> Ok m
      | None -> err

  | RequiredBooleanField _ ->
      match ModelValue.bool ff m with
      | Some _ -> Ok m
      | None -> err

  | RequiredImagePathsField _ ->
        match ModelValue.imagePaths ff m with
        | Some _ -> Ok m
        | None -> err

  | OptionalStringField _ -> Ok m
  | OptionalDateTimeField _ -> Ok m
  | OptionalBooleanField _ -> Ok m
  | OptionalImagePathsField _ -> Ok m

let validateRequiredFieldOnContext ff ctx =
  let k = key ff
  let good = Ok ()
  let contextFields = HttpFormFields.fromContext ctx

  match ff with
  | RequiredStringField _ ->
      good |> HttpFormFields.checkRequiredStringField contextFields k

  | RequiredDateTimeField _ -> good

  | RequiredBooleanField _ -> good

  | RequiredImagePathsField _ -> good

  | OptionalStringField _ -> good
  | OptionalDateTimeField _ -> good
  | OptionalBooleanField _ -> good
  | OptionalImagePathsField _ -> good

let validateFieldOnModel ctx documentType existingItemId field model =
  let res = validatedRequiredFieldOnModel field model

  match (isUnique field), res with
  | true, (Ok m) ->
      match field with
      | RequiredStringField _ -> stringFieldUniquenessValidator ctx documentType existingItemId field m
      | OptionalStringField _ -> stringFieldUniquenessValidator ctx documentType existingItemId field m
      | RequiredDateTimeField _ -> Task.fromResult res
      | RequiredBooleanField _ -> Task.fromResult res
      | RequiredImagePathsField _ -> Task.fromResult res
      | OptionalDateTimeField _ -> Task.fromResult res
      | OptionalBooleanField _ -> Task.fromResult res
      | OptionalImagePathsField _ -> Task.fromResult res
  | _ -> Task.fromResult res

let validateFieldOnContext ctx documentType existingItemId field  =
  let res = validateRequiredFieldOnContext field ctx
  res

let validateFieldsOnModel ctx documentType existingItemId fields model =
  let fields = (viewFieldsForAllFields fields)

  Seq.fold (
    fun res ff ->
      res |> Task.bind (Items.performValidationAsync (validateFieldOnModel ctx documentType existingItemId ff))
      )
    (Task.fromResult (Ok model))
    fields

let validateFieldsOnContext ctx documentType existingItemId fields =
  let fields = viewFieldsForAllFields fields
  Seq.fold (
    fun res ff ->
      res |> Result.bind (fun _ -> validateFieldOnContext ctx documentType existingItemId ff)
      )
    (Ok ())
    fields

let makeJObjectFromModel (m: 'a) (documentType: string) (allFields: seq<FormField<'a>>) =
  let empty = JObj.ofSeq [(Database.documentTypeField, documentType)]
  Seq.fold (fun obj ff -> setJObject m ff obj) empty allFields

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

  open Giraffe.ViewEngine
  let addEditView (m: 'm option) titleCaseDocumentType slug nameField (allFields: seq<FormField<'m>>) allTags documentTags =
    let idField = allFields |> Seq.tryFind (fun ff -> (key ff) = "_id")
    let id = idField |> Option.bind (fun idField -> m |> Option.bind (fun m -> ModelValue.string idField m))

    let pageTitle =
      match m with
      | None -> $"Add %s{titleCaseDocumentType}"
      | Some m ->
          let modelName = (ModelValue.string nameField m) |> Option.defaultValue "ERROR: NAME NOT FOUND"
          $"Edit %s{titleCaseDocumentType} %s{modelName}"

    let pageData =
      match id with
      | Some id -> Map.ofList [ ("id", Items.pageDataType.String id); ("slug", Items.pageDataType.String slug)]
      | None -> Map.empty


    let viewFields = viewFieldsForAllFields allFields
    Items.layout pageTitle pageData [
      yield div [ _class "page-title" ] [ encodedText pageTitle ]
      yield form [ _name "edit-form"; _method "post"; _enctype "multipart/form-data" ] [
          table [] [
              for ff in viewFields do
                  yield makeFormFieldRow ff m

              yield Items.makeTagsInputRow "Tags" Tag.formKey allTags documentTags
              yield tr [] [
                  td [] []
                  td [] [ input [ _type "submit"; _value "Save" ] ]
              ]
          ]
        ]

      if Option.isSome m then
        yield div [ _class "microblog-container section" ] []
    ]
