module ElectricLemur.Muscadine.Site.Database

open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Giraffe
open System.Threading.Tasks
open Newtonsoft.Json.Linq

type ADocument = { _id: string; SomeData: string }

type JsonString = JsonString of string
module JsonString =
    let value (JsonString input) = input
    let from (input: string) = JsonString(input)

module private Mongo =
    open MongoDB.Driver
    open MongoDB.Bson
    
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


    let getDocuments (db: MongoDatabase) = task {
        use! cursor = db.Collection
                        .Find(FilterDefinition<BsonDocument>.Empty)
                        .Sort("{_id: 1}")
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
    
    let getDocument (db: MongoDatabase) (id: string) = task {
        let mutable filter = new BsonDocument()
        filter.["_id"] <- id
        
        let! item = db.Collection.Find(filter).FirstOrDefaultAsync()
        return match item with
                | null -> None
                | _ -> item |> bsonToJObject |> Some
    }
    
    let insertDocument (db: MongoDatabase) document = task {
        let mutable bson =  jObjectToBson document
        let id =
            if bson.Contains("_id") && not (bson.["_id"].IsBsonNull) then
                bson.["_id"].AsString
            else
                bson.["_id"] <- System.Guid.NewGuid().ToString()
                bson.["_id"].AsString
        do! db.Collection.InsertOneAsync(bson)
        return id
    }

let private toJObject o = JObject.Parse(Newtonsoft.Json.JsonConvert.SerializeObject(o))

let private JObjectToADocument (o: JObject) =
    { _id = o.Value<string>("_id"); SomeData = o.Value<string>("SomeData")}

let foo (ctx: HttpContext) = task {
    let! db = Mongo.openDatabase ctx
    let doc = { _id = null; SomeData = "foo bar baz" }
    return! doc |> toJObject |> Mongo.insertDocument db
    
}

let bar (ctx: HttpContext) = task {
    let! db = Mongo.openDatabase ctx
    let! docs = Mongo.getDocuments(db)
    let s = System.String.Join(", ", docs |> Seq.map JObjectToADocument |> Seq.map (fun d -> d.SomeData ))
    return s
}

let baz ctx = task {
    let! db = Mongo.openDatabase ctx
    let! docs = Mongo.getDocuments db
    let first = docs |> Seq.head |> JObjectToADocument
    let! doc = first._id |> Mongo.getDocument db
    return match doc with
            | Some x -> (x |> JObjectToADocument).SomeData
            | None -> "Not Found"
}