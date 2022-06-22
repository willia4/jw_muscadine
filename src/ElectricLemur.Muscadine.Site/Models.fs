module Models
open Newtonsoft.Json.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore.Http

type FieldRequirement =
    | Required
    | NotRequired

type FieldType =
    | Text
    | Boolean

type Field = {
    Key: string
    Label: string
    Required: FieldRequirement
    Type: FieldType
}

type IncludeFields =
    | IncludeDatabaseFields
    | ExcludeDatabaseFields

module DatabaseFields =
    let dateAdded = "dateAdded"
    let id = "_id"

let getKeys (fields: Field seq) includeDatabaseFields = 
    let getKeys requiredType = 
        fields 
        |> Seq.filter (fun f -> f.Required = requiredType) 
        |> Seq.map (fun f -> f.Key)

    let required = getKeys Required
    let optional = getKeys NotRequired

    let required = 
        match includeDatabaseFields with
        | IncludeDatabaseFields -> Util.seqPrepend [ DatabaseFields.id; DatabaseFields.dateAdded ] required
        | ExcludeDatabaseFields -> required

    (required, optional)

type ValidForSave =
    | Valid
    | Invalid of string

let checkValid checker reason res =
    match res with
    | Invalid reason -> Invalid reason
    | Valid -> 
        if checker then 
            Valid
        else 
            Invalid reason

let checkValidAsync (checker: Task<bool>) reason res = task {
    match res with
    | Invalid reason -> return Invalid reason
    | Valid -> 
        let! isValid = checker
        if isValid then 
            return Valid
        else 
            return Invalid reason
}

type Category = 
    {
        DateAdded: System.DateTimeOffset;
        Id: System.Guid;
        ShortName: string;
        LongName: string;
        Description: string;
        Slug: string;
        HasCoverImage: bool;
    }

module Category =
    module Keys =
        let id = DatabaseFields.id
        let dateAdded = DatabaseFields.dateAdded
        let documentType = "documentType"
        let shortName = "shortName"
        let longName = "longName"
        let description = "description"
        let slug = "slug"
        let hasCoverImage = "hasCoverImage"

    let fields = ([
        { Key = Keys.shortName; Label = "Short Name"; Required = Required; Type = Text }
        { Key = Keys.longName; Label = "Long Name"; Required = Required; Type = Text }
        { Key = Keys.description; Label = "Description"; Required = Required; Type = Text }
        { Key = Keys.slug; Label = "Slug"; Required = Required; Type = Text }
        { Key = Keys.hasCoverImage; Label = "Has Cover Image"; Required = NotRequired; Type = Boolean }
    ] |> List.toSeq)

    let documentType = "category"

    let fromJObject (j: JObject) =
        let (requiredFields, optionalFields) = getKeys fields IncludeDatabaseFields

        let values = Util.getJObjectStrings j requiredFields optionalFields

        match values with
        | None -> None
        | Some values -> Some {
            Id = values |> Map.find Keys.id |> Option.get |> Util.guidFromString |> Option.defaultValue (Util.newGuid ())
            DateAdded = values |> Map.find Keys.dateAdded |> Option.get |> Util.dateTimeOffsetFromString |> Option.defaultValue (System.DateTimeOffset.UtcNow)
            ShortName = values |> Map.find Keys.shortName |> Option.get
            LongName = values |> Map.find Keys.longName |> Option.get
            Description = values |> Map.find Keys.description |> Option.get
            Slug = values |> Map.find Keys.slug |> Option.get
            HasCoverImage = values |> Map.find Keys.hasCoverImage |> Option.defaultValue "false" |> Util.boolFromString |> Option.defaultValue false
        }

    let toJObject category =
        let doc = new JObject()
        doc.[Keys.id] <- string category.Id
        doc.[Keys.documentType] <- documentType
        doc.[Keys.dateAdded] <- category.DateAdded.ToString("o")
        doc.[Keys.shortName] <- category.ShortName
        doc.[Keys.longName] <- category.LongName
        doc.[Keys.description] <- category.Description
        doc.[Keys.slug] <- category.Slug
        doc.[Keys.hasCoverImage] <- string category.HasCoverImage
        doc

    let validateForSave (uniquenessChecker: string -> string -> string -> HttpContext -> Task<bool>) ctx category = task {
        let valid = 
            ValidForSave.Valid
            |> checkValid (category.Slug |> (System.String.IsNullOrWhiteSpace >> not)) "Invalid slug"
        
        let id = string category.Id
        let! valid = valid |> checkValidAsync (uniquenessChecker Keys.slug category.Slug id ctx) "Slug must be unique"
        let! valid = valid |> checkValidAsync (uniquenessChecker Keys.shortName category.ShortName id ctx) "Short Name must be unique"
        let! valid = valid |> checkValidAsync (uniquenessChecker Keys.longName category.LongName id ctx) "Long Name must be unique"
        return valid
    }
        