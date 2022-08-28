module HttpFormFields

open Microsoft.AspNetCore.Http

let private requiredFieldError key = $"Required field %s{key} was not present in the form field collection"
let private failRequiredField = requiredFieldError >> failwith 

let fromContext (ctx: HttpContext) = ctx.Request.Form

let containsKey key (formFields: IFormCollection) = formFields.ContainsKey(key)

let stringOptionValue key (formFields: IFormCollection) =
    if formFields |> containsKey key then
        Some (string formFields.[key])
    else
        None

let stringValue key (formFields: IFormCollection) =
    match stringOptionValue key formFields with
    | Some s -> s
    | None -> failRequiredField key

let boolOptionValue key (formFields: IFormCollection) =
    (containsKey key formFields) |> Some

let boolValue key (formFields: IFormCollection) =
    boolOptionValue key formFields |> Option.get

let stringListValue key (formFields: IFormCollection) =
    if formFields |> containsKey key then
        formFields.[key] |> Seq.toList
    else
        []

let checkRequiredField getter isEmpty (formFields: IFormCollection) key prev = 
    match prev with
    | Ok resultValue -> 
        match (getter key formFields) with
        | Some v -> 
            if isEmpty v then
                Error (requiredFieldError key)
            else
                Ok resultValue
        | None -> Error (requiredFieldError key)
    | Error m -> Error m

let checkRequiredStringField (formFields: IFormCollection) key prev = 
    checkRequiredField stringOptionValue System.String.IsNullOrWhiteSpace formFields key prev

