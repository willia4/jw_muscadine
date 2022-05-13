using MongoDB.Bson;
using Newtonsoft.Json;

namespace ElectricLemur.Muscadine.MongoDriver;
using MongoDB.Driver;

public interface IMongoDocument
{
    // ReSharper disable once InconsistentNaming
    public string _id { get; set; }    
}

public interface IDatabase
{
    public string HostName { get; }
    public string DatabaseName { get;  }
    public string CollectionName { get; }
    
    public IMongoClient Client { get; }
    public IMongoDatabase Database { get; }
    public IMongoCollection<BsonDocument> Collection { get; }
}

internal record MongoDatabase<T>(
    string HostName, string DatabaseName, string CollectionName, 
    IMongoClient Client, IMongoDatabase Database, IMongoCollection<BsonDocument> Collection) : IDatabase;

public static class Mongo
{
    public static Task<IDatabase> OpenDatabaseAsync<T>(string hostname, string databaseName, string collectionName, string username, string password)
    {
        var connectionString = $"mongodb://{username}:{password}@{hostname}";
        var client = new MongoClient(connectionString);

        var database = client.GetDatabase(databaseName);
        var collection = database.GetCollection<BsonDocument>(collectionName);
        
        var db = new MongoDatabase<T>(HostName: hostname, DatabaseName: databaseName, CollectionName: collectionName,
            Client: client, Database: database, Collection: collection);

        return Task.FromResult((IDatabase) db);
    }

    public static async Task<IList<T>> GetDocumentsAsync<T>(IDatabase db)
    {
        var results = new List<T>();

        using var cursor = await db.Collection
            .Find(FilterDefinition<BsonDocument>.Empty)
            .Sort("{_id: 1}")
            .ToCursorAsync();

        while (await cursor.MoveNextAsync())
        {
            results.AddRange(cursor.Current.Select(ConvertBson<T>));
        }

        return results;
    }

    public static async Task<T?> GetDocumentAsync<T>(IDatabase db, string id) where T : class
    {
        var d = new BsonDocument
        {
            ["_id"] = id
        };

        var item = await db.Collection.Find(d).FirstOrDefaultAsync();
        return item == null ? null : ConvertBson<T>(item);
    }

    public static async Task<string> InsertDocument<T>(IDatabase db, T document)
    {
        var json = JsonConvert.SerializeObject(document);
        var bson = BsonDocument.Parse(json);
        var id = "";
        
        if (bson.Contains("_id") && !bson["_id"].IsBsonNull)
        {
            id = bson["_id"].AsString;
        }
        else
        {
            id = Guid.NewGuid().ToString();
            bson["_id"] = id;
        }
        
        await db.Collection.InsertOneAsync(bson);
        return id;
    }
    
    private static T ConvertBson<T>(BsonDocument doc)
    {
        return JsonConvert.DeserializeObject<T>(doc.ToJson())!;
    }
}
