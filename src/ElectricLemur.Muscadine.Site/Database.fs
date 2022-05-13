module ElectricLemur.Muscadine.Site.Database

open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Giraffe
open ElectricLemur.Muscadine.MongoDriver

type MongoSettings = { Hostname: string; Database: string; Collection: string; Username: string; Password: string }

type ADocument = { _id: string; SomeData: string }

let private getSettings (ctx: HttpContext) =
    let config = ctx.GetService<IConfiguration>().GetSection("Mongo")
    {
        Hostname = config.GetValue<string>("hostname", "localhost")
        Database = config.GetValue<string>("database", "documents")
        Collection = config.GetValue<string>("collection", "documents")
        Username = config.GetValue<string>("username")
        Password = config.GetValue<string>("password")
    }

let private openDatabase (ctx: HttpContext) = 
    let settings = ctx |> getSettings
    Mongo.OpenDatabaseAsync(settings.Hostname, settings.Database, settings.Collection, settings.Username, settings.Password)
    
let private newGuid = (System.Guid.NewGuid().ToString())
let foo (ctx: HttpContext) = task {
    let! db = openDatabase ctx
    let doc = { _id = null; SomeData = "foo bar baz" }
    let! id = Mongo.InsertDocument(db, doc)
    return id
}

let bar (ctx: HttpContext) = task {
    let! db = openDatabase ctx
    let! docs = Mongo.GetDocumentsAsync<ADocument>(db)
    let s = System.String.Join(", ", docs |> Seq.map (fun d -> d.SomeData ))
    return s
}

let baz ctx = task {
    let! db = openDatabase ctx
    let! docs = Mongo.GetDocumentsAsync<ADocument> db
    let first = docs |> Seq.head
    
    let! doc = Mongo.GetDocumentAsync<ADocument>(db, first._id)
    return doc.SomeData
}