module ElectricLemur.Muscadine.Site.App

open System
open System.IO
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe

// ---------------------------------
// Models
// ---------------------------------

type Message =
    {
        Text : string
    }

// ---------------------------------
// Views
// ---------------------------------

module Views =
    open Giraffe.ViewEngine

    let layout (content: XmlNode list) =
        html [] [
            head [] [
                title []  [ encodedText "ElectricLemur.Muscadine.Site" ]
                link [ _rel  "stylesheet"
                       _type "text/css"
                       _href "/css/main.css" ]
            ]
            body [] content
        ]

    let partial () =
        h1 [] [ encodedText "ElectricLemur.Muscadine.Site" ]

    let index (model : Message) =
        [
            partial()
            p [] [ encodedText model.Text ]
        ] |> layout

// ---------------------------------
// Web app
// ---------------------------------

let indexHandler (name : string) =
    fun (next: HttpFunc) (ctx: HttpContext) -> task {
        let! documentId = Database.foo ctx
        let! joined = Database.bar ctx
        let! found = Database.baz ctx
        let greetings = sprintf "Hello %s, from async Giraffe with some id %s and commas: %s \n\n Found: %s!" name documentId joined found
        let model     = { Text = greetings }
        let view      = Views.index model
        
        return! htmlView view next ctx

    }

let indexHandler2 (name : string) =
    fun (next: HttpFunc) (ctx: HttpContext) ->
        let greetings = sprintf "Hello %s, from Giraffe!" name
        let model     = { Text = greetings }
        let view      = Views.index model
        htmlView view next ctx

let webApp =
    choose [
        GET >=>
            choose [
                route "/" >=> indexHandler "world"
                routef "/hello/%s" indexHandler
            ]
        setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder : CorsPolicyBuilder) =
    builder
        .WithOrigins(
            "http://localhost:5000",
            "https://localhost:5001")
       .AllowAnyMethod()
       .AllowAnyHeader()
       |> ignore


[<EntryPoint>]
let main args =
    let contentRoot = Directory.GetCurrentDirectory()
    let webRoot     = Path.Combine(contentRoot, "WebRoot")
    let builder = WebApplication.CreateBuilder(
        let options = new WebApplicationOptions()
        options.ContentRootPath <- contentRoot
        options.WebRootPath <- webRoot
        options)

    builder.Logging
        .AddConsole()
        .AddDebug() |> ignore

    builder.Services
        .AddCors()
        .AddWebOptimizer(fun pipeline ->
            pipeline.CompileScssFiles(
                let options = new WebOptimizer.Sass.WebOptimazerScssOptions()
                options.MinifyCss <- false
                options) |> ignore
        )
        .AddGiraffe() |> ignore

    let app = builder.Build()
    (match app.Environment.IsDevelopment() with
    | true ->
        app.UseDeveloperExceptionPage()
    | false ->
        app.UseGiraffeErrorHandler(errorHandler)
            .UseHttpsRedirection())
        .UseCors(configureCors)
        .UseWebOptimizer()
        .UseStaticFiles()
        .UseGiraffe(webApp)
        
    app.Run()
    0