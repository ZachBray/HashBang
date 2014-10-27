[<FunScript.JS>]
module Chat.Client.ApplicationState

type AlertType =
    | Success
    | Info
    | Warning
    | Danger
    

let alerts = Event<AlertType * string * string>()
