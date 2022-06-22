module Models
open Newtonsoft.Json.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore.Http

type FieldRequirement =
    | Required
    | NotRequired

type Field = {
    Key: string
    Label: string
    Required: FieldRequirement
}

let getKeys (fields: Field seq) = 
    let getKeys requiredType = fields |> Seq.filter (fun f -> f.Required = requiredType) |> Seq.map (fun f -> f.Key)
    ((getKeys Required), (getKeys NotRequired))

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
    }

module Category =
    module Keys =
        let id = "_id"
        let documentType = "documentType"
        let dateAdded = "dateAdded"
        let shortName = "shortName"
        let longName = "longName"
        let description = "description"
        let slug = "slug";

    let fields = ([
        { Key = Keys.shortName; Label = "Short Name"; Required = Required }
        { Key = Keys.longName; Label = "Long Name"; Required = Required }
        { Key = Keys.description; Label = "Description"; Required = Required }
        { Key = Keys.slug; Label = "Slug"; Required = Required }
    ] |> List.toSeq)

    let documentType = "category"

    let fromJObject (j: JObject) =
        let values = (Util.getJObjectStrings j [ 
            Keys.id
            Keys.dateAdded
            Keys.shortName
            Keys.longName
            Keys.description
            Keys.slug
        ] Seq.empty)

        match values with
        | None -> None
        | Some values -> Some {
            Id = values |> Map.find Keys.id |> Option.get |> Util.guidFromString |> Option.defaultValue (Util.newGuid ())
            DateAdded = values |> Map.find Keys.dateAdded |> Option.get |> Util.dateTimeOffsetFromString |> Option.defaultValue (System.DateTimeOffset.UtcNow)
            ShortName = values |> Map.find Keys.shortName |> Option.get
            LongName = values |> Map.find Keys.longName |> Option.get
            Description = values |> Map.find Keys.description |> Option.get
            Slug = values |> Map.find Keys.slug |> Option.get
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
        