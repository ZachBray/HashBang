namespace Chat.Client.Pages

open System
open FunScript.TypeScript
open FunScript.Rx
open TypeInferred.HashBang
open TypeInferred.HashBang.Html
open TypeInferred.HashBang.Bootstrap
open Chat.Domain.Command
open Chat.Domain
open Chat.Server
open Chat.Client
open Chat.Client.Stylesheets
open Chat.Client.Templates
open Chat.Client.ViewModels

[<FunScript.JS>]
type ConversationPage(navBarPageTemplate : NavBarPageTemplate, conversationViewModel : ConversationViewModel) =
    
    let currentUri = Routes.Conversation.View.CreateUri() 

    let createPage() =
        navBarPageTemplate.Apply
            currentUri
            "Conversation" 
            "Talk to people."
            [
                p [] [] |> Element.appendText ["TODO"]

                FormQuery.puree id
                <*> Inputs.typeahead "User" "joebloggs@gmail.com" 
                        conversationViewModel.FindUsers
                        (fun user -> user.Email)
                        (fun user -> p [] [] |> Element.appendText [user.FirstName + " " + user.SecondName + " | " + user.Email])
                        (Validate.isNotEmpty)  //>> isEmail >> isRegistered)
                |> FormQuery.withSubmitButton "Add" (fun info ->
                    async { return () }
                )
                |> FormQuery.withDisablingFieldset
                |> FormQuery.runBootstrap
            ]
    
    interface IPage with
        member val RequestHandler = 
            Routes.Conversation.View.CreateHandler (fun ps -> 
                async { return Some(createPage()) }
            )