[<FunScript.JS>]
module Chat.Client.ApplicationState

open System
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