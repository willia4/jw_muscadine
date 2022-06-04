module Models
open Newtonsoft.Json.Linq

type Category = 
    {
        DateAdded: System.DateTimeOffset;
        Id: System.Guid;
        ShortName: string;
        LongName: string;
        Description: string;
    }

module Category =
    module Keys =
        let id = "_id"
        let documentType = "documentType"
        let dateAdded = "dateAdded"
        let shortName = "shortName"
        let longName = "longName"
        let description = "description"

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
            Id = Util.guidFromString values.[Keys.id] |> Option.defaultValue (System.Guid.NewGuid());
            DateAdded = Util.dateTimeOffsetFromString values.[Keys.dateAdded] |> Option.defaultValue (System.DateTimeOffset.UtcNow);
            ShortName = values.[Keys.shortName]
            LongName = values.[Keys.longName]
            Description = values.[Keys.description]
        }

    let ToJObject category =
        let doc = new JObject()
        doc.[Keys.id] <- category.Id.ToString()
        doc.[Keys.documentType] <- "category"
        doc.[Keys.dateAdded] <- category.DateAdded.ToString("o")
        doc.[Keys.shortName] <- category.ShortName
        doc.[Keys.longName] <- category.LongName
        doc.[Keys.description] <- category.Description
        doc