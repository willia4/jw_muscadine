module FormFields
open ElectricLemur.Muscadine.Site

type FormField<'m, 'a> =
  | RequiredField of RequiredFields.RequiredFieldDescriptor<'m, 'a>
  | OptionalField of OptionalFields.OptionalFieldDescriptor<'m, 'a>

module FormField =
  let fromRequiredField ff = RequiredField ff
  let fromOptionalField ff = OptionalField ff

let key (field : FormField<_, _>) =
  match field with
  | RequiredField ff -> ff.Key
  | OptionalField ff -> ff.Key

let label (field : FormField<_, _>) =
  match field with
  | RequiredField ff -> ff.Label
  | OptionalField ff -> ff.Label

let formGetter (field : FormField<_, _>) =
  match field with
  | RequiredField ff -> ff.getValueFromContext
  | OptionalField ff -> ff.getValueFromContext

let modelGetter field =
  match field with
  | RequiredField ff -> (ff.getValueFromModel >> Some)
  | OptionalField ff -> ff.getValueFromModel

let jobjGetter field =
  match field with
  | RequiredField ff -> (ff.getValueFromJObject >> Some)
  | OptionalField ff -> ff.getValueFromJObject

let stringFieldUniquenessValidator ctx documentType id field =
  match field with
  | RequiredField ff -> Items.uniqueStringFieldValidator ctx documentType id ff.Key (ff.getValueFromModel >> Some)
  | OptionalField ff -> Items.uniqueStringFieldValidator ctx documentType id ff.Key ff.getValueFromModel

let setJObject model field obj =
  let value = (modelGetter field model)
  JObj.setOptionalValue (key field) value obj

let requiredStringValidator ff =
  let stringGetter m =
    match (modelGetter ff) m with
    | Some v -> string v
    | None -> null

  Items.requiredStringValidator (key ff) stringGetter

module View =
  let modelValue ff m = m |> Option.bind (modelGetter ff)

  let makeTextRow ff m =
    Items.makeTextInputRow (label ff) (key ff) (modelValue ff m)

  let makeTextAreaRow ff lineCount m =
    Items.makeTextAreaInputRow (label ff) (key ff) lineCount (modelValue ff m)

  let makeCheckboxRow ff m =
    Items.makeCheckboxInputRow (label ff) (key ff) (modelValue ff m)

  let makeImageRow (ff: FormField<'a, Image.ImagePaths>) m =
    let v =
      (modelValue ff m)
      |> Option.map (fun paths -> paths.Size512)

    Items.makeImageInputRow (label ff) (key ff) v

