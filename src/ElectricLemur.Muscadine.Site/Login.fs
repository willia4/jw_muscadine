module Login

open System
open System.IO
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Authentication.Cookies
open Microsoft.AspNetCore.Identity
open Microsoft.AspNetCore.Authentication
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open System.Security.Claims
open Giraffe

module private LoginViews =
    open Giraffe.ViewEngine

    let loginForm (errorMessage: string option) (redirect: string option)=
        let emptyDiv = div [ _style "display: none"] []

        html [] [
            head [] [
                title [] [ encodedText "Login" ]
                style [] [ Text "body { 
                                 padding: 15px; font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\"; 
                                 } 
                                 label { font-weight: bold; margin-right: 15px }
                                 #error { display: inline-block; padding-top: 5px; padding-bottom: 5px; 
                                 padding-left: 20px; padding-right: 20px; margin-bottom: 15px;
                                 background-color: #dfaaaa; color: white }"
                                 ]
            ]
            body [] [
                h1 [] [ encodedText "Login" ]
                div [] [
                    form [ 
                        _name "login" 
                        _method "post" ] [
                            match errorMessage with
                            | Some errorMessage -> div [ _id "error" ] [ encodedText errorMessage ]
                            | None -> emptyDiv
                            
                            match redirect with
                            | Some redirect ->  input [ _type "hidden"; _id "redirect"; _name "redirect"; _value redirect ]
                            | None -> emptyDiv

                            table [ ] [
                                tr [] [
                                    td [] [
                                        label [ _for "email" ] [ encodedText "Email" ]
                                    ] 
                                    td [] [
                                        input [ 
                                            _type "text" 
                                            _autofocus
                                            _id "email"
                                            _name "email" 
                                            _autocomplete "email" ]
                                    ]
                                ]

                                tr [] [
                                    td [] [
                                        label [ _for "password" ] [ encodedText "Password" ]
                                    ]
                                    td [] [
                                        input [
                                            _type "password"
                                            _id "password"
                                            _name "password"
                                            _autocomplete "current-password"
                                        ]
                                    ]
                                ]

                                tr [] [
                                    td [ _colspan "2" ] [
                                        input [ 
                                            _type "submit"
                                            _value "Login"
                                            _style "width: 100%"
                                        ]
                                    ]
                                ]
                            ]
                    ]
                ]
            ]
        ]

let getExpectedAdminCredentials (ctx: HttpContext) =
    let configuration = ctx.GetService<IConfiguration>()
    let section = configuration.GetSection("Admin")
    let expectedEmail = section.GetValue<string>("Email")
    let expectedPassword = section.GetValue<string>("Password")
    (expectedEmail, expectedPassword)

let defaultCredentialValidator (expected: string * string) email password = 
    let creds = Option.map2 (fun a b -> (a, b)) email password
    match creds with
    | Some (email, password) -> 
        let (expectedEmail, expectedPassword) = expected

        System.String.Equals(expectedEmail, email, StringComparison.InvariantCultureIgnoreCase) && 
        System.String.Equals(expectedPassword, password, StringComparison.InvariantCulture)
        
    | None -> false

let getQueryStringKey key (ctx: HttpContext) =
    if ctx.Request.Query.ContainsKey(key) then 
        Some (string ctx.Request.Query.[key])
    else
        None

let private redirectIsValid (redirect: string option) =
    let validPatterns = [
        "^/admin$"
        "^/admin/status$"
        "^/admin/check-database$"
        "^/admin/category/[A-Za-z0-9-]*/?$"
        "^/admin/category/_new$"
        "^/admin/game/_new$"
        "^/admin/game/[A-Za-z0-9-]*/?$"
        "^/admin/book/_new$"
        "^/admin/book/[A-Za-z0-9-]*/?$"
        "^/admin/project/_new$"
        "^/admin/project/[A-Za-z0-9-]*/?$"
        "^/admin/microblog/[A-Za-z0-9-]*/?$"
        "^/debug/all$"
    ]

    match redirect with 
    | Some redirect -> 
        validPatterns 
        |> List.tryFind (fun pattern -> System.Text.RegularExpressions.Regex.IsMatch(redirect, pattern)) 
        |> Option.isSome
    | None -> true
        
let getHandler =
    fun next (ctx: HttpContext) ->
        let errorMessage = 
            if ctx.Request.Query.ContainsKey("error") then 
                Some "Invalid username or password"
            else
                None
        let redirect = getQueryStringKey "redirect" ctx
            
        if redirectIsValid redirect then
            htmlView (LoginViews.loginForm errorMessage redirect) next ctx
        else
            RequestErrors.BAD_REQUEST "Invalid redirect" next ctx

let postHandler loginRoute adminRoute (credentialValidator: string option -> string option -> bool) =
    fun (next: HttpFunc) (ctx: HttpContext) -> task {
        let safeGetString = Util.getFormString ctx

        let email = safeGetString "email"
        let password = safeGetString "password"
        let valid = credentialValidator email password

        let redirect = safeGetString "redirect" |> Option.defaultValue adminRoute

        if redirectIsValid (Some redirect) then
            if valid then
                let claims = [ 
                    (Claim(ClaimTypes.Email, (email |> Option.defaultValue "")))
                    (Claim(ClaimTypes.Role, "Admin"))
                ]
                let identity = ClaimsIdentity(claims, CookieAuthenticationDefaults.AuthenticationScheme)
                let claimsPrincipal = ClaimsPrincipal(identity)

                let authProperties = AuthenticationProperties()
                authProperties.AllowRefresh <- true
                authProperties.IsPersistent <- true
                authProperties.IssuedUtc <- System.DateTimeOffset.UtcNow
                authProperties.RedirectUri <- $"%s{loginRoute}?valid"

                do! ctx.SignInAsync(CookieAuthenticationDefaults.AuthenticationScheme, claimsPrincipal, authProperties)

                return! redirectTo false redirect next ctx
            else
                return! redirectTo false $"%s{loginRoute}?error" next ctx
        else 
            return! RequestErrors.BAD_REQUEST "Invalid redirect" next ctx
    }

let logoutHandler loginRoute = 
    fun (next: HttpFunc) (ctx: HttpContext) -> task {
        do! ctx.SignOutAsync(CookieAuthenticationDefaults.AuthenticationScheme)
        return! redirectTo false loginRoute next ctx
    }


let isAdmin (ctx: HttpContext) = ctx.User.IsInRole("Admin")

let requiresAdminRedirect redirect =
    fun next ctx -> requiresRole "Admin" (redirectTo false $"/admin/login?redirect=%s{redirect}") next ctx

let requiresAdminAPICall = 
    fun next ctx -> requiresRole "Admin" (RequestErrors.FORBIDDEN "Access denied") next ctx
           