[<FunScript.JS>]
module TypeInferred.HashBang.SignalR.ServiceProtocol

type CorrelationId = string
type ServiceName = string
type MemberName = string
type TypeArgument = string
type Argument = string
type ServiceCall = ServiceName * MemberName * TypeArgument[] * Argument[]

type ObservableCommand =
    | Subscribe of ServiceCall
    | Unsubscribe

type Command =
    | ObservableCommand of CorrelationId * ObservableCommand
    | AsyncCommand of CorrelationId * ServiceCall

type ObservableEvent =
    | Next of string
    | Error of string
    | Completed

type AsyncEvent =
    | Success of string
    | Failure of string

type Notification =
    | AsyncNotification of CorrelationId * AsyncEvent
    | ObservableNotification of CorrelationId * ObservableEvent