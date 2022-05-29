﻿module Login

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

    let loginForm (errorMessage: string option) =
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
                            | None -> div [] []

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


let getHandler =
    fun next (ctx: HttpContext) ->
        let errorMessage = 
            if ctx.Request.Query.ContainsKey("error") then 
                Some "Invalid username or password"
            else
                None

        htmlView (LoginViews.loginForm errorMessage) next ctx

let postHandler loginRoute (credentialValidator: string option -> string option -> bool) =
    fun (next: HttpFunc) (ctx: HttpContext) -> task {

        let safeGetValue key = 
            if ctx.Request.Form.ContainsKey(key) then
                Some (ctx.Request.Form.[key].ToString())
            else
                None
        
        let email = safeGetValue "email"
        let password = safeGetValue "password"
        let valid = credentialValidator email password

        if valid then
            let claims = [ 
                (new Claim(ClaimTypes.Email, (email |> Option.defaultValue "")))
                (new Claim(ClaimTypes.Role, "Admin"))
            ]
            let identity = new ClaimsIdentity(claims, CookieAuthenticationDefaults.AuthenticationScheme)
            let claimsPrincipal = new ClaimsPrincipal(identity)

            let authProperties = new AuthenticationProperties()
            authProperties.AllowRefresh <- true
            authProperties.IsPersistent <- true
            authProperties.IssuedUtc <- System.DateTimeOffset.UtcNow
            authProperties.RedirectUri <- $"%s{loginRoute}?valid"

            do! ctx.SignInAsync(CookieAuthenticationDefaults.AuthenticationScheme, claimsPrincipal, authProperties)

            return! redirectTo false $"%s{loginRoute}?valid" next ctx
        else
            return! redirectTo false $"%s{loginRoute}?error" next ctx
    }

let logoutHandler loginRoute = 
    fun (next: HttpFunc) (ctx: HttpContext) -> task {
        do! ctx.SignOutAsync(CookieAuthenticationDefaults.AuthenticationScheme)
        return! redirectTo false loginRoute next ctx
    }

let statusHandler = 
    fun (next: HttpFunc) (ctx: HttpContext) ->
        let user = ctx.User
        let role = match user.FindFirst(ClaimTypes.Role) with
                   | null -> "No Role"
                   | claim -> claim.Value

        text role next ctx

let requiresAdmin =
    fun next ctx ->
        requiresRole "Admin" (RequestErrors.FORBIDDEN "Access denied") next ctx