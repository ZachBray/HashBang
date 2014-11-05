namespace Chat.Client.ViewModels

open FunScript

[<JS>]
type AlertType =
    | Success
    | Info
    | Warning
    | Danger

[<JS>]
type AlertsViewModel() =
    let alerts = Event<AlertType * string * string>()
    
    member val NewAlert = alerts.Publish

    member __.Show(alertType, title, message) =
        alerts.Trigger(alertType, title, message)