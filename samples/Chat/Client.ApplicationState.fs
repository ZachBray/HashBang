[<FunScript.JS>]
module Chat.Client.ApplicationState

open System
open TypeInferred.HashBang.Html
open FunScript.TypeScript
open Chat.Domain.Query

type AlertType =
    | Success
    | Info
    | Warning
    | Danger

let alerts = Event<AlertType * string * string>()

type AuthenticationState =
    | LoggedIn of AccessToken
    | NotLoggedIn

let authenticationState = ref NotLoggedIn
let sessionSubscription = ref Disposable.empty

let logOut() =
    let oldSubscription = !sessionSubscription
    sessionSubscription := Disposable.empty
    oldSubscription.Dispose()
    authenticationState := NotLoggedIn
    Globals.location.href <- Routes.Session.LogIn.CreateUri()