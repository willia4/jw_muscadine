module ElectricLemur.Muscadine.Site.Database

open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Giraffe
open System.Threading.Tasks
open Newtonsoft.Json.Linq
open Models

type ADocument = { _id: string; SomeData: string }

type JsonString = JsonString of string
module JsonString =
    let value (JsonString input) = input
    let from (input: string) = JsonString(input)

module private Mongo =
    open MongoDB.Driver
    open MongoDB.Bson
    
    let id = "_id"
    let documentType = "documentType"

    module Filters = 
        type FilterBuilder = {
            EqualTo: seq<string * BsonValue>;
            NotEqualTo: seq<string * BsonValue>;
        }

        let build filterBuilder = 
            let filter = new BsonDocument()
            
            filterBuilder.EqualTo 
            |> Seq.iter (fun (k, v) -> 
                filter.[k] <- v)

            filterBuilder.NotEqualTo
            |> Seq.iter(fun (k, v) ->
                filter.[k] <- new BsonDocument("$ne", v))

            filter

        let addEquals k v current =
            { current with EqualTo = (Seq.append current.EqualTo [ (k, v) ]) }

        let addNotEquals k v current =
            { current with NotEqualTo = (Seq.append current.NotEqualTo [ k, v ]) }

        let empty = { EqualTo = Seq.empty; NotEqualTo = Seq.empty }

        let byMap (m: Map<string, BsonValue>) current =
            m 
            |> Map.toSeq
            |> Seq.fold (fun c (k, v) -> addEquals k v c) current
            
        let by = addEquals
        let byDocumentType (typeName: string) current = by "documentType" typeName current
        let byId (id: string) current = by "_id" id current
        let notId (id: string) current = addNotEquals "_id" id current

    
    module Sort =
        let empty = Filters.empty
        let byId current = Filters.by "_id" 1 current
        let by k current = Filters.by k 1 current
        let build = Filters.build

    type MongoSettings = { Hostname: string; Database: string; Collection: string; Username: string; Password: string }
    type MongoDatabase = { Client: IMongoClient; Database: IMongoDatabase; Collection: IMongoCollection<BsonDocument> }
    let getSettings (ctx: HttpContext) =
        let config = ctx.GetService<IConfiguration>().GetSection("Mongo")
        {
            Hostname = config.GetValue<string>("hostname", "localhost")
            Database = config.GetValue<string>("database", "documents")
            Collection = config.GetValue<string>("collection", "documents")
            Username = config.GetValue<string>("username")
            Password = config.GetValue<string>("password")
        }
    
    let openDatabaseForSettings (settings: MongoSettings) =
        let connectionString = $"mongodb://%s{settings.Username}:%s{settings.Password}@%s{settings.Hostname}"
        let client = new MongoClient(connectionString)
        let database = client.GetDatabase(settings.Database)
        let collection = database.GetCollection<BsonDocument>(settings.Collection)
        
        let r = { Client = client; Database = database; Collection = collection }
        Task.FromResult(r)
    
    let openDatabase: HttpContext -> Task<MongoDatabase> = getSettings >> openDatabaseForSettings

    let private bsonToJson (bson: BsonDocument) = bson.ToJson() |> JsonString.from
    let private jsonToJObject (json: JsonString) = JObject.Parse(json |> JsonString.value)
    let private bsonToJObject = bsonToJson >> jsonToJObject

    let private jsonToBson json = BsonDocument.Parse(json |> JsonString.value)

    let private jObjectToJson (o: JObject) = Newtonsoft.Json.JsonConvert.SerializeObject(o) |> JsonString.from
    let private jObjectToBson = jObjectToJson >> jsonToBson

    let countDocuments (filter: BsonDocument) (db: MongoDatabase) = task {
        return! db.Collection.CountDocumentsAsync(filter)
    }

    let getDocuments (filter: BsonDocument) (sort: BsonDocument) (db: MongoDatabase) = task {
        use! cursor = db.Collection
                        .Find(filter)
                        .Sort(sort)
                        .ToCursorAsync()

        let acc = new System.Collections.Generic.List<JObject>()
        
        let! hasNext = cursor.MoveNextAsync()
        
        let mutable continueLooping = hasNext
        while continueLooping do
            acc.AddRange(cursor.Current |> Seq.map bsonToJObject)   
            let! hasNext = cursor.MoveNextAsync()
            continueLooping <- hasNext
        
        let r: JObject seq = acc
        return r
    }
    
    let getDocument (id: string) (db: MongoDatabase) = task {
        let filter = Filters.empty |> Filters.byId id |> Filters.build
        let! item = db.Collection.Find(filter).FirstOrDefaultAsync()
        return match item with
                | null -> None
                | _ -> item |> bsonToJObject |> Some
    }
    
    let insertDocument document (db: MongoDatabase) = task {
        let bson =  jObjectToBson document
        let id =
            if bson.Contains(id) && not (bson.[id].IsBsonNull) then
                bson.[id].AsString
            else
                bson.[id] <- System.Guid.NewGuid().ToString()
                bson.[id].AsString
        do! db.Collection.InsertOneAsync(bson)
        return id
    }

    let upsertDocument document (db: MongoDatabase) = task {
        let bson = jObjectToBson document
        
        let foundId = 
            match bson.Contains(id) && not (bson.[id].IsBsonNull) with
            | true -> Some bson.[id].AsString
            | false -> None
        
        match foundId with
        | None -> ()
        | Some foundId -> 
            let options = new ReplaceOptions()
            options.IsUpsert <- true
            let filter = Filters.empty |> Filters.byId foundId |> Filters.build
            let! res = db.Collection.ReplaceOneAsync(filter, bson, options)
            ()
    }

    let deleteDocumentById id (db: MongoDatabase) = task {
        let filter = Filters.empty |> Filters.byId id |> Filters.build
        let! res = db.Collection.DeleteOneAsync(filter)
        ()
    }


let getDocumentCount (ctx: HttpContext) (category: string option)= task {
    let! db = Mongo.openDatabase ctx
    let filter = 
        match category with
        | Some c -> Mongo.Filters.empty |> Mongo.Filters.byDocumentType c |> Mongo.Filters.build
        | None -> Mongo.Filters.empty |> Mongo.Filters.build

    return! Mongo.countDocuments filter db
}

let getDocumentsByType documentType (mapper: JObject -> 'a option) ctx = task {
    let! db = Mongo.openDatabase ctx
    let filter = Mongo.Filters.empty |> Mongo.Filters.byDocumentType documentType |> Mongo.Filters.build
    let sort = Mongo.Sort.empty |> Mongo.Sort.by "dateAdded" |> Mongo.Sort.build
    let! documents = db |> Mongo.getDocuments filter sort

    return documents
            |> Seq.map mapper
            |> Seq.filter Option.isSome
            |> Seq.map Option.get
}

let getDocumentById id ctx = task {
    let! db = Mongo.openDatabase ctx
    return! db |> Mongo.getDocument id
}

let getCategories ctx = getDocumentsByType Category.documentType

let insertDocument ctx (document: JObject) = task {
    let! db = Mongo.openDatabase ctx
    return! Mongo.insertDocument document db
}

let upsertDocument ctx (document: JObject) = task {
    let! db = Mongo.openDatabase ctx
    return! Mongo.upsertDocument document db
}

let deleteDocument ctx id = task {
    let! db = Mongo.openDatabase ctx
    do! Mongo.deleteDocumentById id db
}

let checkUniqueness documentType fieldName (fieldValue: string) allowedId ctx = task {
    let! db = Mongo.openDatabase ctx
    let filter = 
        Mongo.Filters.empty
        |> Mongo.Filters.byDocumentType documentType
        |> Mongo.Filters.addEquals fieldName fieldValue
        |> Mongo.Filters.notId allowedId
        |> Mongo.Filters.build
    let sort = Mongo.Sort.empty |> Mongo.Filters.build

    //let! documentCount = db |> Mongo.countDocuments filter
    let! documents = db |> Mongo.getDocuments filter sort 
    let documentCount = Seq.length documents
    return (documentCount = 0)
}