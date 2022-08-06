module ElectricLemur.Muscadine.Site.Database

open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Giraffe
open System.Threading.Tasks
open Newtonsoft.Json.Linq
open System

type ADocument = { _id: string; SomeData: string }

type Limit =
    | Limit of int
    | NoLimit

type JsonString = JsonString of string
module JsonString =
    let value (JsonString input) = input
    let from (input: string) = JsonString(input)

let idField = "_id"
let documentTypeField = "_documentType"

module Filters = 
    open MongoDB.Bson
    type FilterBuilder = 
        {
            EqualTo: seq<string * BsonValue>;
            NotEqualTo: seq<string * BsonValue>;
            In: seq<string * seq<BsonValue>>;
            LessThanEqualDate: seq<string * DateTime>;
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

        filterBuilder.In
        |> Seq.iter(fun (k, vs) ->
            let a = (new BsonArray()).AddRange(vs)
            filter.[k] <- new BsonDocument("$in", a))

        filterBuilder.LessThanEqualDate
        |> Seq.iter(fun (k, v) ->
            filter.[k] <- new BsonDocument("$lte", v))

        filter

    let addEquals k v current =
        { current with EqualTo = (Seq.append current.EqualTo [ (k, v) ]) }

    let addNotEquals k v current =
        { current with NotEqualTo = (Seq.append current.NotEqualTo [ k, v ]) }

    let addIn k (v: BsonValue) current =
        let newIn = 
            current.In 
            |> Map.ofSeq
            |> Map.change k (fun existing -> 
                match existing with
                | Some existing -> existing |> Seq.append [ v ]
                | None -> [ v ]
                |> Some
            )
            |> Map.toSeq
        { current with In = newIn }

    let addLessThanEqualDate k (v: System.DateTimeOffset) current =
        { current with LessThanEqualDate = (Seq.append current.LessThanEqualDate [ (k, v.UtcDateTime) ])}

    let empty = { EqualTo = Seq.empty; NotEqualTo = Seq.empty; In = Seq.empty; LessThanEqualDate = Seq.empty }

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

module private Mongo =
    open MongoDB.Driver
    open MongoDB.Bson



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
        do! createIndex db [ ("_documentType", Ascending); ("itemId", Ascending) ] IndexOptions.None
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

    let private cursorToSeq(cursor: IAsyncCursor<'a>) = task {
        let acc = new System.Collections.Generic.List<'a>()

        let! hasNext = cursor.MoveNextAsync()
        let mutable continueLooping = hasNext
        while continueLooping do
            acc.AddRange(cursor.Current)
            let! hasNext = cursor.MoveNextAsync()
            continueLooping <- hasNext

        return (acc :> seq<'a>)

    }
    let getDocuments (filter: BsonDocument) (sort: BsonDocument) (limit: Limit) (db: MongoDatabase) = task {
        let limit = match limit with
                    | Limit v -> v
                    | NoLimit -> 0

        use! cursor = db.Collection
                        .Find(filter)
                        .Sort(sort)
                        .Limit(limit)
                        .ToCursorAsync()

        let! documents = cursorToSeq cursor
        return (documents |> Seq.map bsonToJObject)
    }
    
    let getDocument (id: string) (db: MongoDatabase) = task {
        let filter = Filters.empty |> Filters.byId id |> Filters.build
        let! item = db.Collection.Find(filter).FirstOrDefaultAsync()
        return match item with
                | null -> None
                | _ -> item |> bsonToJObject |> Some
    }
    
    let getDistinctValues<'a> (field: string) (filter: BsonDocument) (db: MongoDatabase) = task {
        let f: FieldDefinition<BsonDocument, 'a> = field

        use! cursor = db.Collection.DistinctAsync<'a>(field, filter)
        let! docs = cursorToSeq cursor

        return docs 
               //|> Seq.map bsonToJObject
               //|> Seq.map (fun obj -> JObj.getter<'a> obj field)
               //|> Util.unwrapSeqOfOptions
               //|> function
               //   | Some s -> s
               //   | None -> []
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


let getAllDocuments limit (ctx: HttpContext) = task {
    let! db = Mongo.openDatabase ctx
    let filter = Filters.empty |> Filters.build
    let sort = Sort.empty |> Sort.build
    return! Mongo.getDocuments filter sort limit db
}

let getDocumentCount (ctx: HttpContext) (category: string option)= task {
    let! db = Mongo.openDatabase ctx
    let filter = 
        match category with
        | Some c -> Filters.empty |> Filters.byDocumentType c |> Filters.build
        | None -> Filters.empty |> Filters.build

    return! Mongo.countDocuments filter db
}

let getDocumentsForFilterAndSort filter sort limit ctx = task {
    let! db = Mongo.openDatabase ctx

    let filter = filter |> Filters.build
    let sort = sort |> Filters.build

    let! documents = db |> Mongo.getDocuments filter sort limit
    return documents
}

let getDocumentsForFilter filter limit ctx = getDocumentsForFilterAndSort filter Sort.empty limit ctx

let getDocumentsByType documentType (mapper: JObject -> 'a option) limit ctx = task {
    
    let filter = Filters.empty |> Filters.byDocumentType documentType
    let sort = Sort.empty |> Sort.by "dateAdded"
    let! documents = getDocumentsForFilterAndSort filter sort limit ctx
    
    return documents
            |> Seq.map mapper
            |> Seq.filter Option.isSome
            |> Seq.map Option.get
}

let getDocumentById id ctx = task {
    let! db = Mongo.openDatabase ctx
    return! db |> Mongo.getDocument id
}

let getDocumentByTypeAndId documentType id ctx = task {
    let filter =
        Filters.empty
        |> Filters.byDocumentType documentType
        |> Filters.byId id

    let! documents = getDocumentsForFilter filter (Limit 1) ctx
    return documents |> Seq.tryHead
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
        Filters.empty
        |> Filters.byDocumentType documentType
        |> Filters.addEquals fieldName fieldValue
        |> Filters.notId allowedId
        |> Filters.build
    let sort = Sort.empty |> Filters.build

    //let! documentCount = db |> Mongo.countDocuments filter
    let! documents = db |> Mongo.getDocuments filter sort NoLimit
    let documentCount = Seq.length documents
    return (documentCount = 0)
}

let resetIndexes ctx = task {
    let! db = Mongo.openDatabase ctx
    do! Mongo.resetIndexes db
}

let getIdForDocumentTypeAndSlug documentType slug ctx = task {
    let filter = 
        Filters.empty
        |> Filters.byDocumentType documentType
        |> Filters.addEquals "slug" slug

    let! documents = getDocumentsForFilter filter (Limit 1) ctx

    return documents
           |> Seq.tryHead
           |> Option.map (fun d -> JObj.getter<string> d "_id")
           |> Option.flatten
}

let getDistinctValues<'a> field filter ctx = task {
    let! db = Mongo.openDatabase ctx
    return! Mongo.getDistinctValues<'a> field (filter |> Filters.build) db
}