module Models
open Newtonsoft.Json.Linq

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

    let FromJObject (j: JObject) =
        let values = Util.getJObjectStrings j [ 
            Keys.id
            Keys.dateAdded
            Keys.shortName
            Keys.longName
            Keys.description
        ]

        match values with
        | None -> None
        | Some values -> Some {
            Id = values |> Map.find Keys.id |> Util.guidFromString |> Option.defaultValue (Util.newGuid ())
            DateAdded = values |> Map.find Keys.dateAdded |> Util.dateTimeOffsetFromString |> Option.defaultValue (System.DateTimeOffset.UtcNow)
            ShortName = values |> Map.find Keys.shortName
            LongName = values |> Map.find Keys.longName
            Description = values |> Map.find Keys.description
            Slug = j |> JObj.stringValue Keys.slug |> Option.defaultValue ""
        }

    let ToJObject category =
        let doc = new JObject()
        doc.[Keys.id] <- category.Id.ToString()
        doc.[Keys.documentType] <- "category"
        doc.[Keys.dateAdded] <- category.DateAdded.ToString("o")
        doc.[Keys.shortName] <- category.ShortName
        doc.[Keys.longName] <- category.LongName
        doc.[Keys.description] <- category.Description
        doc.[Keys.slug] <- category.Slug
        doc

    let validateForSave category =
        ValidForSave.Valid
        |> checkValid (category.Slug |> System.String.IsNullOrWhiteSpace |> not) "Invalid slug"