module ElectricLemur.Muscadine.Site.App

open System
open System.IO
open System.Linq
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Authentication.Cookies
open Microsoft.AspNetCore.Identity
open Microsoft.Extensions.Configuration
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

    let underConstruction ctx =
        html [] [
            head [] [
                meta [ (_httpEquiv "Content-Type"); (_content "text/html; charset=utf-8") ]
                title [] [ encodedText "James Williams" ]
                (Util.cssLinkTag "landing.scss" ctx)
            ]

            body [] [
                h1 [ _class "page-title" ] [ encodedText "James Williams" ]
                blockquote [] [ encodedText "The future is always all around us, waiting, in moments of transitions, 
                                             to be born in moments of revelation. No one knows the shape of that future 
                                             or where it will take us. We know only that it is always born in pain. " ]
                figcaption [] [ 
                    encodedText "G'kar " 
                    cite [] [ encodedText "Babylon 5, Season 3 " ]
                ]

                blockquote [] [
                    encodedText "Strange fascination, fascinating me "
                    br []
                    br []
                    encodedText "Ah, changes are taking the pace I'm going through. "
                ]

                figcaption [] [ 
                    encodedText "David Bowie " 
                    cite [] [ encodedText "Changes " ]
                ]
            ]
        ]

// ---------------------------------
// Web app
// ---------------------------------

let webApp =
    fun (next: HttpFunc) (ctx: HttpContext) ->
        choose [
            GET >=>
                choose [
                    route "/dev" >=> redirectTo true "/dev/"
                    route "/dev/" >=> Frontend.indexHandler

                    route "/" >=> htmlView (Views.underConstruction ctx)
                    route "/admin/login" >=> Login.getHandler
                    route "/admin/logout" >=> Login.logoutHandler "/admin/login"
                    route "/admin/status" >=> Login.requiresAdminRedirect "/admin/status" >=> Admin.statusHandler
                    route "/admin/check-database" >=> Login.requiresAdminRedirect "/admin/check-database" >=> Admin.checkDatabaseHandler

                    route "/admin/" >=> redirectTo true "/admin"
                    route "/admin" >=> Login.requiresAdminRedirect "/admin" >=> Admin.indexHandler

                    route "/admin/game/_new" >=> Login.requiresAdminRedirect "/admin/game/_new" >=> Game.addHandler_get
                    routef "/admin/game/%s" (fun id -> Login.requiresAdminRedirect $"/admin/game/%s{id} ">=> Game.editHandler_get id)
                    route "/admin/book/_new" >=> Login.requiresAdminRedirect "/admin/book/_new" >=> Book.addHandler_get
                    routef "/admin/book/%s" (fun id -> Login.requiresAdminRedirect $"/admin/book/%s{id} ">=> Book.editHandler_get id)
                    route "/admin/project/_new" >=> Login.requiresAdminRedirect "/admin/project/_new" >=> Project.addHandler_get
                    routef "/admin/project/%s" (fun id -> Login.requiresAdminRedirect $"/admin/project/%s{id} ">=> Project.editHandler_get id)

                    routef "/admin/microblog/%s" (fun id -> Login.requiresAdminRedirect $"/admin/microblog/%s{id}" >=> Microblog.microblogs_edit_get id)

                    route "/debug/all" >=> Login.requiresAdminRedirect "/debug/all" >=> Debug.allDocumentsHandler
                    route "/debug/reset" >=> Login.requiresAdminAPICall >=> Debug.resetDatabase
                    route "/debug/orphaned-tags" >=> Login.requiresAdminAPICall >=> Debug.orphanedTagsGetHandler
                    route "/debug/orphaned-tags/delete" >=> Login.requiresAdminAPICall >=> Debug.orphanedTagsDeleteHandler

                    routef "/game/%s/microblog" (fun id -> Microblog.microblogs_get Game.documentType id)
                    routef "/book/%s/microblog" (fun id -> Microblog.microblogs_get Book.documentType id)
                    routef "/project/%s/microblog" (fun id -> Microblog.microblogs_get Project.documentType id)

                    routexp "/images/(.*?)/(.*?)/(.*?)/(.*)" Image.imageRouter
                ]
            POST >=>
                choose [
                    route "/admin/login" >=> Login.postHandler "/admin/login" "/admin" (Login.defaultCredentialValidator (Login.getExpectedAdminCredentials ctx))

                    routef "/admin/game/%s/microblog" (fun id -> Login.requiresAdminAPICall >=> Microblog.addHandler_post Game.documentType id)
                    routef "/admin/book/%s/microblog" (fun id -> Login.requiresAdminAPICall >=> Microblog.addHandler_post Book.documentType id)
                    routef "/admin/project/%s/microblog" (fun id -> Login.requiresAdminAPICall >=> Microblog.addHandler_post Project.documentType id)

                    route "/admin/game/_new" >=> Login.requiresAdminRedirect "/admin/game/_new" >=> Game.addHandler_post

                    routef "/admin/game/%s" (fun id -> Login.requiresAdminRedirect $"/admin/game/%s{id}" >=> Game.editHandler_post id)
                    route "/admin/book/_new" >=> Login.requiresAdminRedirect "/admin/book/_new" >=> Book.addHandler_post
                    routef "/admin/book/%s" (fun id -> Login.requiresAdminRedirect $"/admin/book/%s{id}" >=> Book.editHandler_post id)
                    route "/admin/project/_new" >=> Login.requiresAdminRedirect "/admin/project/_new" >=> Project.addHandler_post
                    routef "/admin/project/%s" (fun id -> Login.requiresAdminRedirect $"/admin/project/%s{id}" >=> Project.editHandler_post id)

                    routef "/admin/microblog/%s" (fun id -> Login.requiresAdminRedirect $"/admin/microblog/%s{id}" >=> Microblog.microblogs_edit_post id)

                ]
            DELETE >=>
                choose [
                    route "/debug/reset" >=> Login.requiresAdminAPICall >=> Debug.resetDatabaseDeleteHandler

                    routef "/admin/game/%s/microblog/%s" (fun (itemId, blogId) -> Login.requiresAdminAPICall >=> Microblog.microblogs_delete Game.documentType itemId blogId)
                    routef "/admin/book/%s/microblog/%s" (fun (itemId, blogId) -> Login.requiresAdminAPICall >=> Microblog.microblogs_delete Book.documentType itemId blogId)
                    routef "/admin/project/%s/microblog/%s" (fun (itemId, blogId) -> Login.requiresAdminAPICall >=> Microblog.microblogs_delete Project.documentType itemId blogId)

                    routef "/admin/game/%s" (fun id -> Login.requiresAdminRedirect $"/admin/game/%s{id}" >=> Game.deleteHandler_delete id)
                    routef "/admin/book/%s" (fun id -> Login.requiresAdminRedirect $"/admin/book/%s{id}" >=> Book.deleteHandler_delete id)
                    routef "/admin/project/%s" (fun id -> Login.requiresAdminRedirect $"/admin/project/%s{id}" >=> Project.deleteHandler_delete id)
                ]
            setStatusCode 404 >=> text "Not Found" ] next ctx

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
    let webRoot     = Path.Combine(contentRoot, "wwwroot")
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
        .AddGiraffe()
        .AddMemoryCache()
        .AddWebOptimizer(fun pipeline ->
            let options = new WebOptimizer.Sass.WebOptimizerScssOptions()
            let minifyCss = builder.Configuration.GetValue<bool>("minifyCss", true)
            let minifyJavascript = builder.Configuration.GetValue<bool>("minifyJavascript", true)

            options.MinifyCss <- minifyCss
            pipeline.CompileScssFiles(options) |> ignore

            if minifyJavascript then
                pipeline.MinifyJsFiles() |> ignore)
        .AddAuthentication(CookieAuthenticationDefaults.AuthenticationScheme)
        .AddCookie(CookieAuthenticationDefaults.AuthenticationScheme, fun options ->
            options.ExpireTimeSpan <- TimeSpan.FromDays(15)
            options.SlidingExpiration <- true
            options.AccessDeniedPath <- "/Forbidden"
        )
        |> ignore

    let app = builder.Build()

    (match app.Environment.IsDevelopment() with
    | true ->
        app.UseDeveloperExceptionPage()
    | false ->
        app.UseGiraffeErrorHandler(errorHandler)
            .UseHttpsRedirection())
        .UseCors(configureCors)
        .UseWebOptimizer()
    |> ignore

    let staticFilesOptions = new StaticFileOptions()
    staticFilesOptions.OnPrepareResponse <- (fun ctx ->
        let config = ctx.Context.GetService<IConfiguration>()
        let cacheEnabled = config.GetValue<bool>("webOptimizer:enableCaching", false)
        if cacheEnabled then
            let cacheAge = System.TimeSpan.FromDays(30).TotalSeconds |> int

            if (ctx.File.Name.EndsWith(".ttf")) then
                ctx.Context.Response.Headers.Append(
                    "Cache-Control", $"max-age=%d{cacheAge}, public"))

    app
        .UseStaticFiles(staticFilesOptions)
        .UseAuthentication()
        .UseGiraffe(webApp)

    app.Run()
    0