module ElectricLemur.Muscadine.Site.Database

open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Giraffe
open System.Threading.Tasks


type ADocument = { _id: string; SomeData: string }

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

    let private convertFromBson<'a> (d: BsonDocument) =
        let json = d.ToJson()
        Newtonsoft.Json.JsonConvert.DeserializeObject<'a>(json)
        
    let private convertToBson d =
        let json = Newtonsoft.Json.JsonConvert.SerializeObject(d)
        BsonDocument.Parse(json)
        
    let getDocuments<'a> (db: MongoDatabase) = task {
        use! cursor = db.Collection
                        .Find(FilterDefinition<BsonDocument>.Empty)
                        .Sort("{_id: 1}")
                        .ToCursorAsync()

        let acc = new System.Collections.Generic.List<'a>()
        
        let! hasNext = cursor.MoveNextAsync()
        
        let mutable continueLooping = hasNext
        while continueLooping do
            acc.AddRange(cursor.Current |> Seq.map convertFromBson<'a>)   
            let! hasNext = cursor.MoveNextAsync()
            continueLooping <- hasNext
        
        let r: 'a seq = acc
        return r
    }
    
    let getDocument<'a> (db: MongoDatabase) (id: string) = task {
        let mutable filter = new BsonDocument()
        filter.["_id"] <- id
        
        let! item = db.Collection.Find(filter).FirstOrDefaultAsync()
        return match item with
                | null -> None
                | _ -> item |> convertFromBson<'a> |> Some
    }
    
    let insertDocument (db: MongoDatabase) (document: 'a) = task {
        let mutable bson = convertToBson document
        let id =
            if bson.Contains("_id") && not (bson.["_id"].IsBsonNull) then
                bson.["_id"].AsString
            else
                bson.["_id"] <- System.Guid.NewGuid().ToString()
                bson.["_id"].AsString
        do! db.Collection.InsertOneAsync(bson)
        return id
    }

let foo (ctx: HttpContext) = task {
    let! db = Mongo.openDatabase ctx
    let doc = { _id = null; SomeData = "foo bar baz" }
    return! doc |> Mongo.insertDocument db
    
}

let bar (ctx: HttpContext) = task {
    let! db = Mongo.openDatabase ctx
    let! docs = Mongo.getDocuments<ADocument>(db)
    let s = System.String.Join(", ", docs |> Seq.map (fun d -> d.SomeData ))
    return s
}

let baz ctx = task {
    let! db = Mongo.openDatabase ctx
    let! docs = Mongo.getDocuments<ADocument> db
    let first = docs |> Seq.head
    let! doc = first._id |> Mongo.getDocument<ADocument> db
    return match doc with
            | Some x -> x.SomeData
            | None -> "Not Found"
}