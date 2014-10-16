namespace Chat.Client.Pages

open TypeInferred.HashBang
open TypeInferred.HashBang.Html
open Chat.Client
open Chat.Client.Stylesheets

[<FunScript.JS>]
type SignUpPage() =
    
    let currentUri = Routes.Session.SignUp.CreateUri() 
    
    let createPage() =
        Templates.insideNavBar 
            currentUri
            "Sign Up" 
            "Please fill in your details." [
            P.empty --> "TODO: ADD CONTENT"
        ]

    interface IPage with
        member __.TryHandle request = 
            Routes.Session.SignUp.CreateHandler (fun ps -> 
                async { return Some(createPage()) }
            ) request.Path request.QueryParams