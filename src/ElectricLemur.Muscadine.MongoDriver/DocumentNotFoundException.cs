namespace ElectricLemur.Muscadine.MongoDriver;

public class DocumentNotFoundException : Exception
{
    public DocumentNotFoundException(string id) : base($"Could not find document with id {id}") { }
}