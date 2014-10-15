namespace TypeInferred.HashBang.SignalR

open System
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open FunScript
open TypeInferred.HashBang.SignalR.ServiceProtocol

module Scripts =
    
    [<JS; JSEmit("""
var connection = $.connection({6});

connection.received(function (data) { {0}(JSON.stringify(data)); });
connection.error({1});
connection.disconnected({2});
connection.reconnecting({3});
connection.reconnected({4});

connection.start().done(function () {
    console.log("Connected: " + {6});
    {5}(connection);
});""")>]
    let connect(onReceived(*0*):string -> unit,
                onError(*1*):string -> unit,
                onDisconnected(*2*):unit -> unit,
                onReconnecting(*3*):unit -> unit,
                onReconnected(*4*):unit -> unit,
                onConnected(*5*):(obj -> unit) -> unit,
                route(*6*):string) : unit = failwith "never"
        
    [<JSEmitInline("{0}.send({1})")>]
    let send(connection:obj, data:string) : unit = failwith "never"

    [<JSEmitInline("{0}.stop()")>]
    let stop(connection:obj) : unit = failwith "never"

    let getRoute<'TConnection>(dummyVar:'TConnection) = failwith "never"

[<JS>]
type internal SignalRClientConnection<'TConnection, 'TServerMessage, 'TClientMessage 
                                     when 'TConnection :> MessagingConnection<'TServerMessage, 'TClientMessage>>() =
    let mutable connection = None
    let route = Scripts.getRoute(unbox<'TConnection> null)
    let connected = Event<_>()
    let received = Event<_>()
    let error = Event<_>()
    let disconnected = Event<_>()
    //let connectionSlow = Event<_>()
    let reconnecting = Event<_>()
    let reconnected = Event<_>()
    
    let deserialize = Serialization.precomputeFromJson()
    let serialize = Serialization.precomputeToJson()
    member __.IsConnected = Option.isSome connection
    member val Connected = connected.Publish :> System.IObservable<_>
    member val Received = received.Publish :> System.IObservable<'TClientMessage>
    member val Error = error.Publish :> System.IObservable<_>
    member val Disconnected = disconnected.Publish :> System.IObservable<_>
    //member val ConnectionSlow = connectionSlow.Publish :> System.IObservable<_>
    member val Reconnecting = reconnecting.Publish :> System.IObservable<_>
    member val Reconnected = reconnected.Publish :> System.IObservable<_>

    member __.Connect() =
        match connection with
        | Some _ -> ()
        | None ->
            Scripts.connect(
                (fun data -> received.Trigger(deserialize data)),
                error.Trigger,
                (fun () -> 
                    connection <- None
                    disconnected.Trigger()),
                reconnecting.Trigger,
                reconnected.Trigger,
                (fun conn -> 
                    connection <- Some conn
                    connected.Trigger()),
                route)

    member __.Send (msg : 'TServerMessage) =
        match connection with
        | None -> failwith "Not connected"
        | Some c -> Scripts.send(c, serialize msg)

    member __.Stop() =
        match connection with
        | None -> failwith "Not connected"
        | Some c -> Scripts.stop(c)

[<FunScript.JS>]
module internal SerializerCaches =
    let serializers = Dictionary()
    let deserializers = Dictionary()

[<FunScript.JS>]
type internal SerializerCache<'T>() =
    static member CreateSerializer() =
        let typeKey = typeof<'T>.FullName
        if not (SerializerCaches.serializers.ContainsKey typeKey) then
            let serializer = TypeInferred.HashBang.SignalR.Serialization.precomputeToJson<'T>()
            SerializerCaches.serializers.Add(typeKey, serializer :> obj)
            serializer
        else unbox<'T -> string> SerializerCaches.serializers.[typeKey]

    static member CreateDeserializer() =
        let typeKey = typeof<'T>.FullName
        if not (SerializerCaches.deserializers.ContainsKey typeKey) then
            let deserializer = TypeInferred.HashBang.SignalR.Serialization.precomputeFromJson<'T>()
            SerializerCaches.deserializers.Add(typeKey, deserializer :> obj)
            deserializer
        else unbox<string -> 'T> SerializerCaches.deserializers.[typeKey]
        
    static member Serialize(obj : 'T) =
        let serialize = SerializerCache<'T>.CreateSerializer()
        serialize obj

    static member DeserializeObservable jsonUpdates =
        let deserialize = SerializerCache<'T>.CreateDeserializer()
        jsonUpdates |> Observable.map deserialize

    static member DeserializeAsync jsonAsync =
        async {
            let deserialize = SerializerCache<'T>.CreateDeserializer()
            let! json = jsonAsync
            return deserialize json
        }

[<FunScript.JS>]
module GuidEx = //TODO: Add support for Guids to FunScript.
    
    [<JSEmit("""
return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
    var r = Math.random()*16|0, v = c == 'x' ? r : (r&0x3|0x8);
    return v.toString(16);
}); """)>]
    let random() = System.Guid.NewGuid().ToString() 
        

[<FunScript.JS>]
module ObservableEx = // TODO: Create FunScript mapping from Rx.NET to rx.js

    type private CreateObservable<'T>(f: IObserver<'T> -> IDisposable) =
        interface IObservable<'T> with
            member x.Subscribe(observer) = f observer

    let create f = CreateObservable(f) :> IObservable<'T>

//    type private ActionObserver<'T> (onNext : 'T -> unit, onError : exn -> unit, onCompleted : unit -> unit) =    
//        interface IObserver<'T> with
//             member this.OnNext v = onNext v
//             member this.OnError e = onError e
//             member this.OnCompleted() = onCompleted()
//
//    let subscribe onNext onError onCompleted (xs : IObservable<'T>) =
//        xs.Subscribe(ActionObserver(onNext, onError, onCompleted))

[<FunScript.JS>]
module DisposableEx =

    let create f = new FunScript.Core.Events.ActionDisposable(f) :> IDisposable

    let empty() = create ignore

    type SerialDisposable() =
        let mutable isDisposed = false
        let mutable disposable = empty()
        let dispose() =
            if not isDisposed then 
                isDisposed <- true
                disposable.Dispose()
                disposable <- empty()
        member __.Dispose() = dispose()
        member __.Disposable 
                with get() = disposable
                and set(v : IDisposable) =
                    if isDisposed then v.Dispose()
                    else 
                        let oldDisposable = disposable
                        disposable <- v
                        oldDisposable.Dispose()
        interface IDisposable with
            member __.Dispose() = dispose()


[<FunScript.JS>]
module internal ServicesConnection =
    let connection = lazy SignalRClientConnection<ServiceProtocolConnection, ServiceProtocol.Command, ServiceProtocol.Notification>()    

    let subscribe(serviceName, memberName, typeArgs, args) =
        let connection = connection.Value
        ObservableEx.create(fun observer ->
            let correlationId = GuidEx.random()
            let resources = new DisposableEx.SerialDisposable()
            let hasCompleted = ref false
            let subscribe() = 
                connection.Send(ObservableCommand(correlationId, Subscribe(serviceName, memberName, typeArgs, args)))
                let updateSubscription =
                    connection.Received |> Observable.subscribe(function
                        | ObservableNotification(id, notification) when id = correlationId -> 
                            match notification with
                            | Next x -> observer.OnNext x
                            | Error msg -> 
                                observer.OnError(exn msg)
                                hasCompleted := true
                                resources.Dispose()
                            | Completed -> 
                                observer.OnCompleted()
                                hasCompleted := true
                                resources.Dispose()
                        | _ -> ())
                let unsubscribe() =
                    if not !hasCompleted then
                        connection.Send(ObservableCommand(correlationId, Unsubscribe))
                resources.Disposable <- DisposableEx.create(fun () -> 
                    updateSubscription.Dispose()
                    unsubscribe())
            if connection.IsConnected then subscribe()
            else   
                resources.Disposable <- connection.Connected |> Observable.subscribe (fun () -> subscribe())
                connection.Connect()
            resources :> IDisposable)

    let asyncCall(serviceName, memberName, typeArgs, args) =
        let connection = connection.Value
        Async.FromContinuations(fun (onSuccess, onError, _) ->
            let correlationId = GuidEx.random()
            let resources = new DisposableEx.SerialDisposable()
            let subscribe() = 
                connection.Send(AsyncCommand(correlationId, (serviceName, memberName, typeArgs, args)))
                resources.Disposable <-
                    connection.Received |> Observable.subscribe(function
                        | AsyncNotification(id, notification) when id = correlationId -> 
                            match notification with
                            | Success x -> 
                                resources.Dispose()
                                onSuccess x
                            | Failure msg -> 
                                resources.Dispose()
                                onError(exn msg)
                        | _ -> ())
            if connection.IsConnected then subscribe()
            else
                resources.Disposable <- connection.Connected |> Observable.subscribe (fun () -> subscribe())
                connection.Connect()
        )

module Interop =

    let internal serializeExpr t objExpr =
        let cacheType = typedefof<SerializerCache<_>>.MakeGenericType [|t|]
        let methodInfo = cacheType.GetMethod("Serialize", BindingFlags.NonPublic ||| BindingFlags.Static)
        Expr.Call(methodInfo, [objExpr])

    let internal deserializeAsyncExpr t jsonExpr =
        let cacheType = typedefof<SerializerCache<_>>.MakeGenericType [|t|]
        let methodInfo = cacheType.GetMethod("DeserializeAsync", BindingFlags.NonPublic ||| BindingFlags.Static)
        Expr.Call(methodInfo, [jsonExpr])

    let internal deserializeObservableExpr t jsonExpr =
        let cacheType = typedefof<SerializerCache<_>>.MakeGenericType [|t|]
        let methodInfo = cacheType.GetMethod("DeserializeObservable", BindingFlags.NonPublic ||| BindingFlags.Static)
        Expr.Call(methodInfo, [jsonExpr])

    let internal iobservableName = typeof<_ IObservable>.Name
    let internal asyncName = typeof<_ Async>.Name

    let internal createServiceCall (memberInfo : MemberInfo, returnType : Type, typeArgs : Type[], argExpressions : Expr list) =
        let serviceName = memberInfo.DeclaringType.FullName
        let memberName = memberInfo.Name
        let argArrayExpression = 
            let serializedArgExpressions =
                argExpressions |> List.map (fun argExpression -> 
                    serializeExpr argExpression.Type argExpression)
            Expr.NewArray(typeof<string>, serializedArgExpressions)
        let typeArgsArrayExpression = 
            let typeArgExpressions = 
                typeArgs 
                |> Array.map (fun t -> Expr.Value t.FullName)
                |> Array.toList
            Expr.NewArray(typeof<string>, typeArgExpressions)
        match returnType.GetGenericArguments() with
        | [|itemType|] ->
            match returnType.Name with
            | n when n = asyncName -> 
                let serializedAsyncResult =
                    <@ ServicesConnection.asyncCall(
                            serviceName, memberName, 
                            %%typeArgsArrayExpression, 
                            %%argArrayExpression) @>
                deserializeAsyncExpr itemType serializedAsyncResult
                |> Some
                //|> compiler.Compile returnStrategy
            | n when n = iobservableName ->
                let serializedIObservable =
                    <@ ServicesConnection.subscribe(
                            serviceName, memberName, 
                            %%typeArgsArrayExpression, 
                            %%argArrayExpression) @>
                deserializeObservableExpr itemType serializedIObservable
                |> Some
                //|> compiler.Compile returnStrategy
            | _ -> None
        | _ -> None

    let components = [
        CompilerComponent.unaryTyped 
            <@ Scripts.getRoute @>
            (fun t _ ->
                t.GetCustomAttributes(false) |> Seq.tryPick (fun attribute ->
                    match attribute with
                    | :? RouteAttribute as routeAttribute -> Some(AST.JSExpr.String routeAttribute.Path)
                    | _ -> None  
                ))

        CompilerComponent.create (fun _ compiler returnStrategy expr ->
            match expr with
            | Patterns.Call(Some _, methodInfo, argExpressions) 
              when methodInfo.DeclaringType.GetCustomAttribute<ServiceAttribute>() <> Unchecked.defaultof<_> ->
                let callExpr = createServiceCall(methodInfo, 
                                                 methodInfo.ReturnType, 
                                                 methodInfo.GetGenericArguments(), 
                                                 argExpressions)
                match callExpr with
                | None -> []
                | Some expr -> compiler.Compile returnStrategy expr
            | Patterns.PropertyGet(Some _, propInfo, argExpressions) 
              when propInfo.DeclaringType.GetCustomAttribute<ServiceAttribute>() <> Unchecked.defaultof<_> ->
                let callExpr = createServiceCall(propInfo, 
                                                 propInfo.PropertyType, 
                                                 [||], 
                                                 argExpressions)
                match callExpr with
                | None -> []
                | Some expr -> compiler.Compile returnStrategy expr
            | _ -> [])
    ]