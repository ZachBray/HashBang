namespace Chat.Client.Pages

open System
open FunScript.TypeScript
open FunScript.Rx
open TypeInferred.HashBang
open TypeInferred.HashBang.Html
open TypeInferred.HashBang.Bootstrap
open Chat.Domain.Command
open Chat.Domain
open Chat.Client
open Chat.Client.Stylesheets
open Chat.Server
open Chat.Client.ApplicationState

[<FunScript.JS>]
type ConversationPage(authService : IAuthenticationService) =
    
    let currentUri = Routes.Conversation.View.CreateUri() 

    let createPage() =
        Templates.insideNavBar 
            currentUri
            "Conversation" 
            "Talk to people."
            [
                p [] [] |> Element.appendText ["TODO"]
            ]
    
    interface IPage with
        member val RequestHandler = 
            Routes.Conversation.View.CreateHandler (fun ps -> 
                async { return Some(createPage()) }
            )