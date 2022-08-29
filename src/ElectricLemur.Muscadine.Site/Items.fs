module ElectricLemur.Muscadine.Site.Items

open Microsoft.AspNetCore.Http
open System.Threading.Tasks
open Giraffe.ViewEngine
open Microsoft.FSharp.Reflection
open Newtonsoft.Json.Linq

let performValidationAsync (f: 'a -> Task<Result<'a, string>>) (prev: Result<'a, string>) =
    match prev with
    | Error s -> Task.fromResult (Error s)
    | Ok g -> task {
        return! f g
    }

type pageDataType =
    | String of value: string
    | Int of value: int
    | Float of value: float

let layout pageTitle pageData content =
    let makePageDataScript =
        let sb = new System.Text.StringBuilder()
        let sb = sb.AppendLine("window.pageData = {};")

        let sb =
            pageData
            |> Map.fold (fun (sb: System.Text.StringBuilder) k v ->

                let sb = sb.Append($"window.pageData[\"%s{k}\"] = ")
                let sb = sb.Append(match v with
                                   | String s -> $"\"%s{s}\""
                                   | Int i -> $"%d{i}"
                                   | Float f -> $"%f{f}")
                sb.AppendLine(";")) sb
        sb.ToString()

    html [] [
        head [] [
            title [] [ encodedText pageTitle ]
            link [ (_rel "stylesheet"); (_type "text/css"); (_href "/css/admin.scss") ]
            script [] [ rawText makePageDataScript ]
            script [ _src "/js/admin.js" ] []
        ]
        body [] [
            div [ _class "site-title" ] [
                encodedText "James Williams/"
                a [ _href "/admin/" ] [encodedText "Admin"]
            ]
            div [ _class "body-content" ] content
        ]
    ]

let makeInputRow label formEl = 
    tr [] [
        td [ _class "form-label" ] [ encodedText label ]
        td [ _class "form-input" ] [ formEl ]
    ]

let makeImageInputRow label key pathToDisplay =
    let src = 
        match Util.addRootPath "/images" pathToDisplay with 
        | Some p -> p
        | None -> ""

    let el = div 
                [ _class "image-input"
                  attr "data-image-key" key ]
                [ 
                    input [ 
                        _type "file" 
                        _style "display: none"
                        _name key
                        _id key
                    ]
                    img [
                        _src src
                    ] 
                ]
    makeInputRow label el

let makeTextAreaInputRow label key lineCount value =
    let el =
        textarea [
            _id key
            _name key
            _rows $"%d{lineCount}"
            _cols "30"
        ] [ encodedText (value |> Option.defaultValue "")]

    makeInputRow label el

let makeTextInputRow label key value =
    let el = input [
        _type "text"
        _id key
        _name key
        _value (value |> Option.defaultValue "")
    ]
    makeInputRow label el

let makeCheckboxInputRow label key (value: bool option) = 
    let value = value |> Option.defaultValue false
    let attributes = 
        [
            _type "checkbox"
            _id key
            _name key
            _value "on"
        ]
        |> List.prependIf value _checked

    let el = input attributes
    makeInputRow label el

let makeTagsInputRow label key (allTags: string seq) (documentTags: string seq) =
    let selectAttributes = [
        _name key
        _id key
        _multiple
        _size "10"
    ]

    let allTags = Seq.append allTags documentTags |> Seq.distinct |> Seq.sort |> Seq.toList
    let tagIsSelected t = documentTags |> Seq.contains t

    let el = 
        div [ _class "tags-container" ] [
            select selectAttributes (
                allTags
                |> List.map (fun t -> 
                    let attributes = 
                        [ _value t ]
                        |> List.prependIf (tagIsSelected t) _selected

                    option attributes [ encodedText t ]))

            div [ _class "new-tag-container"] [
                input [ _type "text"; _class "new-tag-field"; _id "new-tag-field"; _name "new-tag-field" ]
                button [ _class "new-tag-button" ] [ encodedText "Add Tag"]
            ]
        ]
    makeInputRow label el

let uniqueStringFieldValidator ctx documentType allowedId fieldName (getter: 'a -> string option) (m: 'a) = task {
    match getter m with
    | None -> return Ok m
    | Some v ->
        let! valid = Database.checkUniqueness documentType fieldName v allowedId ctx
        match valid with
        | true -> return Ok m
        | false -> return Error $"%s{fieldName} must be unique"
}

let handleFileUpload ctx documentType id key (existingPath: string option) (modelSetter: string option -> 'a) = task {
    let files = Util.uploadedFiles ctx
    let file = files |> List.filter (fun f -> f.Name = key) |> List.tryHead

    let! filePath =
        match file with
        | None -> Task.fromResult (Ok existingPath)
        | Some file -> Util.saveFileToDataStore file documentType id key ctx

    return match filePath with
           | Error msg -> Error msg
           | Ok coverImagePath -> Ok (modelSetter coverImagePath)
}

let handleImageUpload ctx documentType documentId key (existingPath: Image.ImagePaths option) (modelSetter: Image.ImagePaths option -> 'a) = task {
    let files = Util.uploadedFiles ctx
    let originalFile = files |> List.filter (fun f -> f.Name = key) |> List.tryHead

    match originalFile with
    | None -> return Ok (modelSetter existingPath)
    | Some file -> 
        let! o = Image.saveImageToDataStore file documentType documentId key ctx
        return match o with
               | Error msg -> Error msg
               | Ok newPaths -> Ok (modelSetter (Some newPaths))

}

let getLinkToItem itemDocumentType slug ctx =
    match itemDocumentType with
    | s when s = Constants.Database.DocumentTypes.Book -> Some "books"
    | s when s = Constants.Database.DocumentTypes.Project -> Some "projects"
    | s when s = Constants.Database.DocumentTypes.Game -> Some "games"
    | _ -> None
    |> Option.map (fun itemDocumentType ->
        Util.makeUrl $"/%s{itemDocumentType}/%s{slug}/" ctx |> string)

let getDefaultIcon documentType =
    match documentType with
    | s when s = Constants.Database.DocumentTypes.Book -> Constants.Icons.Book
    | s when s = Constants.Database.DocumentTypes.Project -> Constants.Icons.Project
    | s when s = Constants.Database.DocumentTypes.Game -> Constants.Icons.Game
    | _ -> Constants.Icons.QuestionMark
    |> Image.FontAwesome

let tryReadItemImagePaths (itemData: JObject option) =
    match itemData with
    | Some itemData -> "coverImage" |> JObj.getter<Image.ImagePaths> itemData
    | None -> None

let tryReadName (itemData: JObject option) =
    itemData
    |> Option.choosef [
      (fun obj -> Option.bind (fun obj -> JObj.getter<string> obj "name") obj)
      (fun obj -> Option.bind (fun obj -> JObj.getter<string> obj "title") obj)
    ]

let tryReadSlug (itemData: JObject option) =
    itemData
    |> Option.bind (fun obj -> JObj.getter<string> obj "slug")

let tryReadDocumentType (itemData: JObject option) =
    itemData
    |> Option.bind (fun itemData -> JObj.getter<string> itemData Database.documentTypeField)

let readItemImageOrDefault itemData =
    itemData
    |> tryReadItemImagePaths
    |> function
       | Some imagePaths -> Image.Image imagePaths
       | None ->
            itemData
            |> tryReadDocumentType
            |> Option.defaultValue ""
            |> getDefaultIcon

let readNameOrDefault (itemData: JObject option) = itemData |> tryReadName |> Option.defaultValue "Unknown"
