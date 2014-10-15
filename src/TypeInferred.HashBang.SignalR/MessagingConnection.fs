namespace TypeInferred.HashBang.SignalR

open System.Threading.Tasks
open Microsoft.AspNet.SignalR
open Microsoft.AspNet.SignalR.Messaging

type internal MessagingConnection<'TServerMessage, 'TClientMessage>() =
    inherit PersistentConnection()

    let getConnectionId id = "c-" + id
    let serialize = Serialization.precomputeToJson()
    let deserialize = Serialization.precomputeFromJson()

    member this.Broadcast(data:'TClientMessage) =
        let payload = serialize data
        use writer = new System.IO.StringWriter()
        this.JsonSerializer.Serialize(writer, payload)
        let escapedPayload = writer.ToString()
        let msg = Message(this.Transport.ConnectionId, this.Connection.DefaultSignal, escapedPayload)
        this.MessageBus.Publish(msg)
        |> Async.AwaitIAsyncResult
        |> Async.Ignore

    member this.SendMany(connectionIds, data:'TClientMessage) =
        let fromConnectionId = this.Transport.ConnectionId
        let messageBus = this.MessageBus
        let payload = serialize data
        connectionIds |> Array.map (fun id ->
            let connectionId = getConnectionId id
            let msg = Message(fromConnectionId, connectionId, payload)
            messageBus.Publish msg)
        |> Task.WhenAll
        |> Async.AwaitIAsyncResult
        |> Async.Ignore

    member this.Send(connectionId, data) =
        this.SendMany([|connectionId|], data)

    abstract OnMessage : IRequest * string * 'TServerMessage -> unit Async
    default this.OnMessage(request, connectionid, data) = async { return () }

    override this.OnReceived(request, connectionId, data) =
        let input : 'TServerMessage = deserialize data
        this.OnMessage(request, connectionId, input)
        |> Async.StartAsTask :> Task