namespace Chat.Client.ViewModels

open System
open FunScript
open FunScript.TypeScript
open FunScript.Rx
open TypeInferred.HashBang.Html
open Chat.Domain.Query
open Chat.Client
open Chat.Server
open Chat.Client.ViewModels

[<JS>]
type AuthenticationViewModel(alerts : AlertsViewModel, authService : IAuthenticationService) =
    let mutable sessionSubscription = Disposable.empty
    let mutable accessTokenAndUser = None

    member __.GetAccessTokenAsync() =
        Async.FromContinuations(fun (onSuccess, onError, _) ->
            match accessTokenAndUser with
            | None -> 
                Globals.location.href <- Routes.Session.LogIn.CreateUri()
                alerts.Show(Danger, "Not logged in", "Please log in again.")
            | Some token -> onSuccess token
        )

    member __.GetAccessTokenObservable() =
        Observable.defer(fun () ->
            match accessTokenAndUser with
            | None ->
                Globals.location.href <- Routes.Session.LogIn.CreateUri()
                alerts.Show(Danger, "Not logged in", "Please log in again.")
                Observable.empty
            | Some token -> Observable.result token)
            
    member __.IsLoggedIn = accessTokenAndUser |> Option.isSome

    member __.IsEmailRegistered email = authService.IsEmailRegistered email

    member __.LogIn(email, password) =
        Async.FromContinuations(fun (onValue, onError, _) ->
            sessionSubscription <-
                authService.LogIn(email, password) 
                |> Observable.subscribeWithCallbacks
                    (fun token -> 
                        accessTokenAndUser <- Some token
                        Globals.location.href <- Routes.Conversation.View.CreateUri()
                        onValue())
                    (fun ex -> 
                        alerts.Show(Danger, "Failed to log in.", ex.Message)
                        onValue())
                    (fun () -> alerts.Show(Danger, "Server connection interrupted.", "Please refresh the page."))
        )

    member __.LogOut() =
        let oldSubscription = sessionSubscription
        sessionSubscription <- Disposable.empty
        oldSubscription.Dispose()
        accessTokenAndUser <- None
        Globals.location.href <- Routes.Session.LogIn.CreateUri()

    member __.SignUp userDetails =
        Async.FromContinuations(fun (onValue, onError, _) ->
            sessionSubscription <-
                authService.SignUp(userDetails) 
                |> Observable.subscribeWithCallbacks
                    (fun token -> 
                        accessTokenAndUser <- Some token 
                        Globals.location.href <- Routes.Conversation.View.CreateUri()
                        onValue())
                    (fun ex -> 
                        alerts.Show(Danger, "Failed to sign up.", ex.Message)
                        onValue())
                    (fun () ->
                        alerts.Show(Danger, "Server connection interrupted.", "Please refresh the page."))
        )