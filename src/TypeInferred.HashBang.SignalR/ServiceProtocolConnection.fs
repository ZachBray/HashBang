namespace TypeInferred.HashBang.SignalR

open System
open System.Collections.Concurrent
open TypeInferred.HashBang.SignalR.ServiceProtocol

[<Route("/services")>]
type internal ServiceProtocolConnection(services : ServiceCollection) =
    inherit MessagingConnection<Command, Notification>()

    let connectionsSubscriptions = Cache(fun _ -> ConcurrentDictionary())

    override connection.OnMessage(request, connectionId, command) =
        async {
            match command with
            | ObservableCommand(correlationId, Subscribe serviceCall) ->
                let subscriptions = connectionsSubscriptions.FindOrCreate connectionId
                let sendMessageScheduler = 
                    let isComplete = ref false
                    SerialAsyncCallScheduler(fun notification ->
                        if !isComplete then
                            async { return () }
                        else
                            match notification with
                            | Error _ | Completed -> isComplete := true
                            | _ -> ()
                            connection.Send(connectionId, ObservableNotification(correlationId, notification))) //TODO: catch and log failure here
                try 
                    let updates = services.CallObservableMember serviceCall
                    
                    let subscription =
                        lazy
                            updates.Subscribe
                                {   new IObserver<string> with 
                                        member __.OnNext serializedUpdate = 
                                            sendMessageScheduler.Enqueue(Next serializedUpdate)
                                        member __.OnError ex =
                                            sendMessageScheduler.Enqueue(Error ex.Message) // TODO: Log?
                                            subscriptions.TryRemove(correlationId) |> ignore
                                        member __.OnCompleted() =
                                            sendMessageScheduler.Enqueue(Completed)
                                            subscriptions.TryRemove(correlationId) |> ignore
                                }
                    if subscriptions.TryAdd(correlationId, subscription) then
                        // Here we create the subscription. We do this after adding it to the dictionary so that
                        // synchronous errors and completions still remove the subscription.
                        subscription.Force() |> ignore 
                    else failwithf "There is already a subscription with correlationId '%A'." correlationId
                with ex -> sendMessageScheduler.Enqueue(Error ex.Message)
            | ObservableCommand(correlationId, Unsubscribe) ->
                let subscriptions = connectionsSubscriptions.FindOrCreate connectionId
                match subscriptions.TryRemove correlationId with
                | false, _ -> () // TODO: log?
                | true, subscription -> subscription.Value.Dispose()
            | AsyncCommand(correlationId, serviceCall) ->
                try
                    let! result = services.CallAsyncMember serviceCall
                    do! connection.Send(connectionId, AsyncNotification(correlationId, Success result))
                with ex ->
                    try do! connection.Send(connectionId, AsyncNotification(correlationId, Failure ex.Message))
                    with _ -> () // TODO: log failure to send failure here.
        }

    override this.OnDisconnected(request, connectionId) =
        match connectionsSubscriptions.TryRemove connectionId with
        | None -> ()
        | Some subscriptions ->
            subscriptions |> Seq.iter (fun (KeyValue(_, subscription)) -> subscription.Value.Dispose())
        base.OnDisconnected(request, connectionId)