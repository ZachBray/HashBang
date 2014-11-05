namespace Chat.Client.Pages

open System
open FunScript.Rx
open FunScript.TypeScript
open TypeInferred.HashBang
open TypeInferred.HashBang.Html
open TypeInferred.HashBang.Bootstrap
open Chat.Domain.Query
open Chat.Domain
open Chat.Client
open Chat.Client.Stylesheets
open Chat.Client.ApplicationState
open Chat.Server

[<FunScript.JS>]
type LogInInfo =
    {
        Email : string
        Password : string
        ShouldRememberUser : bool
    }
        

[<FunScript.JS>]
type LogInPage(authService : IAuthenticationService) =

    let currentUri = Routes.Session.LogIn.CreateUri() 

    let isEmail = Validate.checkThat Validation.isEmail "Must be a valid email."

    let isRegistered = Validate.checkThatAsync authService.IsEmailRegistered "Must be a registered email."

    let createPage() =
        Templates.insideNavBar
            currentUri
            "Log In" 
            "Please fill in your email address and password or sign-up for a new account." [
            FormQuery.puree (fun email password shouldRememberUser -> 
                {
                    Email = email
                    Password = password
                    ShouldRememberUser = shouldRememberUser
                })
            <*> Inputs.text "Email" "joebloggs@gmail.com" (Validate.isNotEmpty >> isEmail >> isRegistered)
            <*> Inputs.password "Password" "my_s3cr3t" (Validate.isAtLeast 8)
            <*> Inputs.checkbox "Remember me?" id
            |> FormQuery.withSubmitButton "Log in" (fun info ->
                Async.FromContinuations(fun (onValue, onError, _) ->
                    authService.LogIn(info.Email, info.Password) 
                    |> Observable.subscribeWithCallbacks
                        (fun token -> 
                            authenticationState := LoggedIn token
                            Globals.location.href <- Routes.Conversation.View.CreateUri()
                            onValue())
                        (fun ex -> 
                            ApplicationState.alerts.Trigger(Danger, "Failed to log in.", ex.Message)
                            onValue())
                        (fun () -> ApplicationState.alerts.Trigger(Danger, "Server connection interrupted.", "Please refresh the page."))
                    |> ignore //TODO: capture for log out
                ))
            |> FormQuery.withDisablingFieldset
            |> FormQuery.runBootstrap
        ]

    interface IPage with
        member __.TryHandle request = 
            Routes.Session.LogIn.CreateHandler (fun ps -> 
                async { return Some(createPage()) }
            ) request.Path request.QueryParams