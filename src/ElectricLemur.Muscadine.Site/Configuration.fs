module ElectricLemur.Muscadine.Site.Configuration

open System
open Giraffe
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration

let getConfig (ctx: HttpContext) = ctx.GetService<IConfiguration>()

let getSection key (config: IConfiguration) = config.GetSection(key)

let tryGetValue<'a> key (section: IConfiguration) =
    // IConfiguration is weird since GetSection always returns something, but the thing it returns might contain a null value.
    // So we can use that to determine if the key exists or not.
    
    // If it does exist, we then need to convert that value into a typed object. There's a bunch of private stuff in ConfigurationBinder
    // to do that (up to and including deserializing from base64 strings) that we do not want to reimplement
    // so we will just call `GetValue` if we know the value exists. It will redo the same check we are doing  but it shouldn't be too bad
    // as long as we don't call tryGetValue in a hot loop (don't do that!)
    //
    // We can't call GetValue first because it returns default('a) if the key doesn't exist so it's not possible to know if the key
    // does not exist or exists and is null 
    let found = section.GetSection(key)
    if found.Value = null then
        None
    else
        section.GetValue<'a>(key)
        |> Some
    
let getValue<'a> key (section: IConfiguration) =
    let v = tryGetValue<'a> key section
    match v with
    | Some v -> v
    | None ->
        let path = 
            match section with
            | :? IConfigurationSection as section -> section.Path
            | _ -> "/"
            
        raise (InvalidOperationException($"Could not find key {key} in config section {path}"))

