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
        let byMap (m: Map<string, BsonValue>) = 
            let filter = new BsonDocument()
            m |> Map.iter (fun k v -> filter.[k] <- v)
            filter

        let empty = byMap Map.empty

        let by k v = byMap (Map [(k, v)])

        let byDocumentType (typeName: string) = by "documentType" typeName

        let byId (id: string) = by "_id" id
    
    module Sort =
        let empty = Filters.empty
        let byId = Filters.by "_id" 1
        let by k = Filters.by k 1

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
        let! item = db.Collection.Find(Filters.byId id).FirstOrDefaultAsync()
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
            let! res = db.Collection.ReplaceOneAsync(Filters.byId foundId, bson, options)
            ()
    }

    let deleteDocumentById id (db: MongoDatabase) = task {
        let! res = db.Collection.DeleteOneAsync(Filters.byId id)
        ()
    }


let getDocumentCount (ctx: HttpContext) (category: string option)= task {
    let! db = Mongo.openDatabase ctx
    let filter = 
        match category with
        | Some c -> Mongo.Filters.byDocumentType c
        | None -> Mongo.Filters.empty

    return! Mongo.countDocuments filter db
}

let getDocumentsByType documentType (mapper: JObject -> 'a option) ctx = task {
    let! db = Mongo.openDatabase ctx
    let! documents = db |> Mongo.getDocuments (Mongo.Filters.byDocumentType documentType) (Mongo.Sort.by "dateAdded")

    return documents
            |> Seq.map mapper
            |> Seq.filter Option.isSome
            |> Seq.map Option.get
}

let getDocumentById id ctx = task {
    let! db = Mongo.openDatabase ctx
    return! db |> Mongo.getDocument id
}

let getCategories ctx = getDocumentsByType "category"

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