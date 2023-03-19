module ElectricLemur.Muscadine.Site.App

open System
open System.IO
open System.Linq
open Microsoft.AspNetCore.CookiePolicy
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Authentication.Cookies
open Microsoft.AspNetCore.DataProtection
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

let commonHeaders = [
    "X-Clacks-Overhead", "GNU Terry Pratchett"
    "X-Hire-Me", "https://jameswilliams.me/resume"
    "X-Source-Code", "https://github.com/willia4/jw_muscadine"
    "X-Greeting", "Hello! I hope you are having an amazing day. We are all rooting for you."
]

let webApp =
    fun (next: HttpFunc) (ctx: HttpContext) ->
        let setCommonHeaders  =
            let doNothing (next: HttpFunc) (ctx: HttpContext) : HttpFuncResult =
                next ctx

            commonHeaders
            |> List.fold (fun n (k, v) -> n >=> setHttpHeader k v) doNothing

        choose [
            GET >=> setCommonHeaders >=> (
                let makeItemRoutes singularName pluralName documentType indexHandler microblogHandlerBuilder itemPageHandlerBuilder =
                    // this is difficult to do because routef needs a PrintfFormat and not a string.
                    // and because interpolated strings can't use %s just by themselves
                    
                    let pctS= "%s"
                    // "/books/%s/microblogs/%s"
                    // "/books/%s/microblogs/%s/"
                    let microblogRedirect = Microsoft.FSharp.Core.PrintfFormat<_,_,_,_, string*string> ($"/%s{pluralName}/%s{pctS}/microblogs/%s{pctS}")
                    let microblog = Microsoft.FSharp.Core.PrintfFormat<_,_,_,_, string*string> ($"/%s{pluralName}/%s{pctS}/microblogs/%s{pctS}/")
                    
                    // "/books/%s"
                    // "/books/%s/"
                    let itemRedirect = Microsoft.FSharp.Core.PrintfFormat<_,_,_,_, string> ($"/%s{pluralName}/%s{pctS}")
                    let item = Microsoft.FSharp.Core.PrintfFormat<_,_,_,_, string> ($"/%s{pluralName}/%s{pctS}/")
                    
                    // "/book/%s/microblog"
                    let allMicroblogsByItemId = Microsoft.FSharp.Core.PrintfFormat<_,_,_,_, string> ($"/%s{singularName}/%s{pctS}/microblog")
                    
                    [
                        route $"/%s{pluralName}" >=> redirectTo true $"/%s{pluralName}/"
                        route $"/%s{pluralName}/" >=> indexHandler
                        
                        routef microblogRedirect (fun (slug, microblogId) -> redirectTo true $"/%s{pluralName}/%s{slug}/microblogs/%s{microblogId}/")
                        routef microblog microblogHandlerBuilder
                        
                        routef itemRedirect (fun slug -> redirectTo true $"/%s{pluralName}/%s{slug}/")
                        routef item itemPageHandlerBuilder
                        
                        routef allMicroblogsByItemId (fun id -> Microblog.Handlers.GET_list documentType id)
                        
                        route $"/feed/microblogs/%s{pluralName}" >=> redirectTo true $"/feed/microblogs/%s{pluralName}/"
                        route $"/feed/microblogs/%s{pluralName}/" >=> (Microblog.Handlers.GET_atomFeed (Some Game.documentType) pluralName (Util.makeUrl $"/feed/microblogs/pluralName/"))
                    ]

                let gameRoutes =
                    makeItemRoutes
                        "game" "games" Game.documentType
                        Frontend.Game.Handlers.GET_index (fun (slug, mbid) -> ItemHelper.Handlers.GET_microblogPage ItemHelper.ItemDocumentType.GameDocumentType slug mbid)
                        Frontend.Game.Handlers.GET_itemPage

                let bookRoutes =
                    makeItemRoutes
                        "book" "books" Book.documentType
                        Frontend.Book.Handlers.GET_index (fun (slug, mbid) -> ItemHelper.Handlers.GET_microblogPage ItemHelper.ItemDocumentType.BookDocumentType slug mbid)
                        Frontend.Book.Handlers.GET_itemPage
                        
                let projectRoutes =
                    makeItemRoutes
                        "project" "projects" Project.documentType
                        Frontend.Project.Handlers.GET_index (fun (slug, mbid) -> ItemHelper.Handlers.GET_microblogPage ItemHelper.ItemDocumentType.ProjectDocumentType slug mbid)
                        Frontend.Project.Handlers.GET_itemPage

                let miscRoutes = [
                    route "/" >=> Frontend.AboutMe.Handlers.GET_index
                    route "/under-construction" >=> redirectTo true "/under-construction/"
                    route "/under-construction/" >=> htmlView (Views.underConstruction ctx)
                    
                    route "/resume" >=> redirectTo true "/resume/"
                    route "/resume/" >=> Resume.GET

                    route "/dev" >=> redirectTo true "/about/"
                    route "/dev/" >=> redirectTo true "/about/"
                    
                    route "/colophon" >=> redirectTo true "/colophon/"
                    route "/colophon/" >=> Frontend.Colophon.Handlers.GET_index
                    
                    route "/about" >=> redirectTo true "/about/"
                    route "/about/" >=> Frontend.AboutMe.Handlers.GET_index
                    
                    route "/updates" >=> redirectTo true "/updates/"
                    route "/updates/" >=> Frontend.AboutMe.Handlers.GET_all
                    routef "/updates/%s/" (fun slug -> Frontend.AboutMe.Handlers.GET_allForItemType slug)
                    routef "/updates/%s" (fun slug -> redirectTo true $"/updates/%s{slug}/")
                    
                    routexp "/images/(.*?)/(.*?)/(.*?)/(.*)" Image.Handlers.GET_imageRouter
                    
                    route "/feed/microblogs" >=> redirectTo true "/feed/microblogs/"
                    route "/feed/microblogs/" >=> (Microblog.Handlers.GET_atomFeed None "everything" (Util.makeUrl "/feed/microblogs/"))
                ]
                
                let adminRoutes = [
                    route "/admin/login" >=> Login.getHandler
                    route "/admin/logout" >=> Login.logoutHandler "/admin/login"
                    route "/admin/status" >=> Login.requiresAdminRedirect "/admin/status" >=> Admin.Handlers.GET_status
                    route "/admin/check-database" >=> Login.requiresAdminRedirect "/admin/check-database" >=> Admin.Handlers.GET_checkDatabase

                    route "/admin/" >=> redirectTo true "/admin"
                    route "/admin" >=> Login.requiresAdminRedirect "/admin" >=> Admin.Handlers.GET_index

                    route "/admin/game/_new" >=> Login.requiresAdminRedirect "/admin/game/_new" >=> Admin.Game.Handlers.GET_add
                    routef "/admin/game/%s" (fun id -> Login.requiresAdminRedirect $"/admin/game/%s{id} ">=> Admin.Game.Handlers.GET_edit id)
                    route "/admin/book/_new" >=> Login.requiresAdminRedirect "/admin/book/_new" >=> Admin.Book.Handlers.GET_add
                    routef "/admin/book/%s" (fun id -> Login.requiresAdminRedirect $"/admin/book/%s{id} ">=> Admin.Book.Handlers.GET_edit id)
                    route "/admin/project/_new" >=> Login.requiresAdminRedirect "/admin/project/_new" >=> Admin.Project.Handlers.GET_add
                    routef "/admin/project/%s" (fun id -> Login.requiresAdminRedirect $"/admin/project/%s{id} ">=> Admin.Project.Handlers.GET_edit id)

                    routef "/admin/microblog/%s" (fun id -> Login.requiresAdminRedirect $"/admin/microblog/%s{id}" >=> Microblog.Handlers.GET_edit id)
                ]

                let debugRoutes = [
                    route "/debug/all" >=> Login.requiresAdminRedirect "/debug/all" >=> Debug.Handlers.GET_allDocuments
                    route "/debug/reset" >=> Login.requiresAdminAPICall >=> Debug.Handlers.GET_resetDatabase
                    route "/debug/orphaned-tags" >=> Login.requiresAdminAPICall >=> Debug.Handlers.GET_orphanedTags
                    route "/debug/orphaned-tags/delete" >=> Login.requiresAdminAPICall >=> Debug.Handlers.DELETE_orphanedTags
                ]
                
                let routes = [
                    yield! gameRoutes
                    yield! bookRoutes
                    yield! projectRoutes
                    yield! miscRoutes
                    yield! adminRoutes
                    yield! debugRoutes
                ]
                
                choose routes)
            POST >=> setCommonHeaders >=>
                choose [
                    route "/admin/login" >=> Login.postHandler "/admin/login" "/admin" (Login.defaultCredentialValidator (Login.getExpectedAdminCredentials ctx))

                    routef "/admin/game/%s/microblog" (fun id -> Login.requiresAdminAPICall >=> Microblog.Handlers.POST_add Game.documentType id)
                    routef "/admin/book/%s/microblog" (fun id -> Login.requiresAdminAPICall >=> Microblog.Handlers.POST_add Book.documentType id)
                    routef "/admin/project/%s/microblog" (fun id -> Login.requiresAdminAPICall >=> Microblog.Handlers.POST_add Project.documentType id)

                    route "/admin/game/_new" >=> Login.requiresAdminRedirect "/admin/game/_new" >=> Admin.Game.Handlers.POST_add
                    routef "/admin/game/%s" (fun id -> Login.requiresAdminRedirect $"/admin/game/%s{id}" >=> Admin.Game.Handlers.POST_edit id)

                    route "/admin/book/_new" >=> Login.requiresAdminRedirect "/admin/book/_new" >=> Admin.Book.Handlers.POST_add
                    routef "/admin/book/%s" (fun id -> Login.requiresAdminRedirect $"/admin/book/%s{id}" >=> Admin.Book.Handlers.POST_edit id)

                    route "/admin/project/_new" >=> Login.requiresAdminRedirect "/admin/project/_new" >=> Admin.Project.Handlers.POST_add
                    routef "/admin/project/%s" (fun id -> Login.requiresAdminRedirect $"/admin/project/%s{id}" >=> Admin.Project.Handlers.POST_edit id)

                    routef "/admin/microblog/%s" (fun id -> Login.requiresAdminRedirect $"/admin/microblog/%s{id}" >=> Microblog.Handlers.POST_edit id)

                ]
            DELETE >=> setCommonHeaders >=>
                choose [
                    route "/debug/reset" >=> Login.requiresAdminAPICall >=> Debug.Handlers.DELETE_resetDatabase

                    routef "/admin/game/%s/microblog/%s" (fun (itemId, blogId) -> Login.requiresAdminAPICall >=> Microblog.Handlers.DELETE Game.documentType itemId blogId)
                    routef "/admin/book/%s/microblog/%s" (fun (itemId, blogId) -> Login.requiresAdminAPICall >=> Microblog.Handlers.DELETE Book.documentType itemId blogId)
                    routef "/admin/project/%s/microblog/%s" (fun (itemId, blogId) -> Login.requiresAdminAPICall >=> Microblog.Handlers.DELETE Project.documentType itemId blogId)

                    routef "/admin/game/%s" (fun id -> Login.requiresAdminRedirect $"/admin/game/%s{id}" >=> ItemHelper.AdminHandlers.DELETE id)
                    routef "/admin/book/%s" (fun id -> Login.requiresAdminRedirect $"/admin/book/%s{id}" >=> ItemHelper.AdminHandlers.DELETE id)
                    routef "/admin/project/%s" (fun id -> Login.requiresAdminRedirect $"/admin/project/%s{id}" >=> ItemHelper.AdminHandlers.DELETE id)
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
        let options = WebApplicationOptions(
            ContentRootPath = contentRoot,
            WebRootPath = webRoot)
        options)

    builder.Logging
        .AddConsole()
        .AddDebug() |> ignore

    let services = builder.Services
    services
        .AddDataProtection()
        .PersistKeysToAzureBlobStorage(
            Azure.StorageAccount.getBlobSasUriForBlob "data-protection" "data-protection" builder.Configuration)
        .ProtectKeysWithAzureKeyVault((
            let uri = $"https://%s{Azure.getKeyVaultName builder.Configuration}.vault.azure.net/keys/data-protection/" 
            Uri uri),
             Azure.getAzureCredentials builder.Configuration)
        .SetApplicationName("jameswilliams-me")
        |> ignore

    services
        .AddCors()
        .AddGiraffe()
        .AddMemoryCache()
        .AddWebOptimizer(fun pipeline ->
            let options = WebOptimizer.Sass.WebOptimizerScssOptions()
            let minifyCss = builder.Configuration.GetValue<bool>("minifyCss", true)
            let minifyJavascript = builder.Configuration.GetValue<bool>("minifyJavascript", true)

            options.MinifyCss <- minifyCss
            pipeline.CompileScssFiles(options) |> ignore

            if minifyJavascript then
                pipeline.MinifyJsFiles() |> ignore)
        .AddAuthentication(CookieAuthenticationDefaults.AuthenticationScheme)
        .AddCookie(CookieAuthenticationDefaults.AuthenticationScheme, fun options ->
            options.ExpireTimeSpan <- TimeSpan.FromDays(15.0 * 365.0)
            options.SlidingExpiration <- true
            options.AccessDeniedPath <- "/Forbidden"
            options.Cookie.Name <- "jameswilliams-me-auth"
            options.Cookie.IsEssential <- true
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

    let staticFilesOptions = StaticFileOptions()
    staticFilesOptions.OnPrepareResponse <- (fun ctx ->
        let isImage (fileName: string) =
            let fileName = fileName.ToLowerInvariant()
            fileName.EndsWith(".ico") || fileName.EndsWith(".jpg") || fileName.EndsWith(".png")
            
        let config = ctx.Context.GetService<IConfiguration>()
        let cacheEnabled = config.GetValue<bool>("webOptimizer:enableCaching", false)
        if cacheEnabled then
            let fileName = ctx.File.Name

            if fileName.EndsWith(".ttf") || (isImage fileName) then
                let cacheAge = TimeSpan.FromDays(30).TotalSeconds |> int

                ctx.Context.Response.Headers.Append(
                    "Cache-Control", $"max-age=%d{cacheAge}, public")
        
        Util.contentTypeForFileName ctx.File.Name
        |> Option.iter (fun contentType -> ctx.Context.Response.Headers.ContentType <- contentType)
        
        commonHeaders
        |> List.iter (fun (k, v) ->
            ctx.Context.Response.Headers.Append(k, v)))

    app
        .UseStaticFiles(staticFilesOptions)
        .UseAuthentication()
        .UseGiraffe(webApp)

    app.Run()
    0