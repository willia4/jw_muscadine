module ElectricLemur.Muscadine.Site.Azure
open System
open Azure.Storage
open Azure.Storage.Blobs
open Azure.Storage.Sas
open ElectricLemur.Muscadine.Site
open Microsoft.Extensions.Configuration

let private getAzureSection (config: IConfiguration) = config |> Configuration.getSection "Azure"
let private getDataProtectionSection (config: IConfiguration) = config |> getAzureSection |> Configuration.getSection "DataProtection"

let private getCredentials (config: IConfiguration) =
    let credentialsSection =
        config
        |> getAzureSection
        |> Configuration.getSection "Identity"

    {|
        TenantId = credentialsSection |> Configuration.getValue<string> "TenantId"
        ClientId = credentialsSection |> Configuration.getValue<string> "Id"
        Secret = credentialsSection |> Configuration.getValue<string> "Secret"
    |}
    
let getAzureCredentials (config: IConfiguration) =
    let credentials = getCredentials config
    
    Azure.Identity.ClientSecretCredential(credentials.TenantId, credentials.ClientId, credentials.Secret)
    :> Azure.Core.TokenCredential
    
let getKeyVaultName (config: IConfiguration) =
    config
    |> getDataProtectionSection
    |> Configuration.getValue<string> "KeyVault"
    
module StorageAccount =
    let private getStorageAccountSection (config: IConfiguration) =
        config
        |> getDataProtectionSection
        |> Configuration.getSection "StorageAccount"
        
    let getStorageAccountName (config: IConfiguration) =
        config
        |> getStorageAccountSection
        |> Configuration.getValue<string> "Name"
        
    let getStorageAccountKey (config: IConfiguration) =
        config
        |> getStorageAccountSection
        |> Configuration.getValue<string> "Key"
        
    let getBlobConnectionString (config: IConfiguration) =
        let name = getStorageAccountName config
        let key = getStorageAccountKey config
        
        $"DefaultEndpointsProtocol=https;AccountName={name};AccountKey={key};EndpointSuffix=core.windows.net"

    let getBlobSasUriForBlob container blob config =
        let name = getStorageAccountName config
        let key = getStorageAccountKey config
        
        let containerUri = Uri $"https://%s{name}.blob.core.windows.net/%s{container}"
        let storageCredential = StorageSharedKeyCredential (name, key)
        let containerClient = BlobContainerClient (containerUri, storageCredential)
        
        
        let sasBuilder = BlobSasBuilder()
        sasBuilder.BlobContainerName <- container
        sasBuilder.BlobName <- blob
        sasBuilder.Resource <- "b"
        sasBuilder.Protocol <- SasProtocol.Https
        sasBuilder.ExpiresOn <- DateTimeOffset.UtcNow.AddDays(365)
        sasBuilder.SetPermissions(BlobSasPermissions.All)
        
        let blobClient = containerClient.GetBlobClient(blob)
        blobClient.GenerateSasUri(sasBuilder)
        
       

