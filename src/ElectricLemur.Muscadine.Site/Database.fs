module ElectricLemur.Muscadine.Site.Database

open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Giraffe
open System.Threading.Tasks
open Newtonsoft.Json.Linq
open System

type ADocument = { _id: string; SomeData: string }

type JsonString = JsonString of string
module JsonString =
    let value (JsonString input) = input
    let from (input: string) = JsonString(input)

module private Mongo =
    open MongoDB.Driver
    open MongoDB.Bson
    
    let idField = "_id"
    let documentTypeField = "_documentType"

    module Filters = 
        type FilterBuilder = 
            {
                EqualTo: seq<string * BsonValue>;
                NotEqualTo: seq<string * BsonValue>;
            } 
            override this.ToString() = 
                ""

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
        let byDocumentType (typeName: string) current = by documentTypeField typeName current
        let byId (id: string) current = by idField id current
        let notId (id: string) current = addNotEquals idField id current

    
    module Sort =
        let empty = Filters.empty
        let byId current = Filters.by idField 1 current
        let by k current = Filters.by k 1 current
        let build = Filters.build

    type MongoSettings = { Hostname: string; Database: string; Collection: string; Username: string; Password: string }
    type MongoDatabase = { Client: IMongoClient; Database: IMongoDatabase; Collection: IMongoCollection<BsonDocument> }
    let private getSettings (ctx: HttpContext) =
        let config = ctx.GetService<IConfiguration>().GetSection("Mongo")
        {
            Hostname = config.GetValue<string>("hostname", "localhost")
            Database = config.GetValue<string>("database", "documents")
            Collection = config.GetValue<string>("collection", "documents")
            Username = config.GetValue<string>("username")
            Password = config.GetValue<string>("password")
        }
    
    let private openDatabaseForSettings (settings: MongoSettings) =
        let connectionString = $"mongodb://%s{settings.Username}:%s{settings.Password}@%s{settings.Hostname}"
        let client = new MongoClient(connectionString)
        let database = client.GetDatabase(settings.Database)
        let collection = database.GetCollection<BsonDocument>(settings.Collection)
        
        let r = { Client = client; Database = database; Collection = collection }
        Task.FromResult(r)
    
    type private IndexDirection = 
        | Ascending
        | Descending

    [<Flags>]
    type IndexOptions =
        | None = 0
        | Unique = 1

    let rec private createIndex db (keys: (string * IndexDirection) seq) (options: IndexOptions)= task {
        let keyDefinition = new BsonDocument()
        keys |> Seq.iter (fun (k, dir) ->
            keyDefinition.[k] <- match dir with 
                                 | Ascending -> 1
                                 | Descending -> -1
        )

        let opt = new CreateIndexOptions()
        opt.Unique <- (options &&& IndexOptions.Unique = IndexOptions.Unique)

        let m = new CreateIndexModel<BsonDocument>(keyDefinition, opt)
        let c = db.Collection

        try
            let! _ = c.Indexes.CreateOneAsync(m)
            return ()
        with
        | :? MongoDB.Driver.MongoCommandException as ex when ex.Code = 86 -> // code 86 is "index exists with a different definition" so delete it and try again
            let indexName = ex.Command.["indexes"].AsBsonArray[0].AsBsonDocument["name"].AsString
            do! c.Indexes.DropOneAsync(indexName)
            return! createIndex db keys options
    }

    let resetIndexes db = task {
        let c = db.Collection
        do! c.Indexes.DropAllAsync()
    }

    let private initDatabase db = task {
        do! createIndex db [ ("_documentType", Ascending) ] IndexOptions.None
        do! createIndex db [ ("slug", Ascending); ("_documentType", Ascending) ] IndexOptions.None
        do! createIndex db [ ("_documentType", Ascending); ("slug", Ascending) ] IndexOptions.None
        return db
    }

    let openDatabase (ctx: HttpContext): Task<MongoDatabase> = task {
        let! db = openDatabaseForSettings (getSettings ctx)
        
        return! initDatabase db
    }

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
            if bson.Contains(idField) && not (bson.[idField].IsBsonNull) then
                bson.[idField].AsString
            else
                bson.[idField] <- System.Guid.NewGuid() |> string
                bson.[idField].AsString
        do! db.Collection.InsertOneAsync(bson)
        return id
    }

    let upsertDocument document (db: MongoDatabase) = task {
        let bson = jObjectToBson document
        
        let foundId = 
            match bson.Contains(idField) && not (bson.[idField].IsBsonNull) with
            | true -> Some bson.[idField].AsString
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


let getAllDocuments (ctx: HttpContext) = task {
    let! db = Mongo.openDatabase ctx
    let filter = Mongo.Filters.empty |> Mongo.Filters.build
    let sort = Mongo.Sort.empty |> Mongo.Sort.build
    return! Mongo.getDocuments filter sort db
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

let checkUniqueness documentType fieldName (fieldValue: MongoDB.Bson.BsonValue) allowedId ctx = task {
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

let resetIndexes ctx = task {
    let! db = Mongo.openDatabase ctx
    do! Mongo.resetIndexes db
}

let getIdForDocumentTypeAndSlug documentType slug ctx = task {
    let! db = Mongo.openDatabase ctx
    let filter = 
        Mongo.Filters.empty
        |> Mongo.Filters.byDocumentType documentType
        |> Mongo.Filters.addEquals "slug" slug
        |> Mongo.Filters.build
    let sort = Mongo.Sort.empty |> Mongo.Filters.build

    let! documents = db |> Mongo.getDocuments filter sort

    return documents
           |> Seq.tryHead
           |> Option.map (fun d -> JObj.getter<string> d "_id")
           |> Option.flatten
}