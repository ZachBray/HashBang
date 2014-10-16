namespace Chat.Client.Pages

open TypeInferred.HashBang
open TypeInferred.HashBang.Html
open Chat.Client
open Chat.Client.Stylesheets

[<FunScript.JS>]
type LogInPage() =
    
    let createPage() =
        Templates.insideNavBar 
            "Log In" 
            "Please fill in your email address and password or sign-up for a new account." [
            P.empty --> "TODO: ADD CONTENT"
        ]

    interface IPage with
        member __.TryHandle request = 
            Routes.Session.LogIn.CreateHandler (fun ps -> 
                async { return Some(createPage()) }
            ) request.Path request.QueryParams