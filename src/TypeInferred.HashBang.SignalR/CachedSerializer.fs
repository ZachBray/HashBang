namespace TypeInferred.HashBang.SignalR

open System

type internal CachedSerializer<'T>() =
    static let serializer = lazy Serialization.precomputeToJson()
    static let deserializer = lazy Serialization.precomputeFromJson()

    static member SerializeAsync(obj : 'T Async) = 
        async {
            let! x = obj
            return serializer.Value x
        }

    static member SerializeObservable(xs : 'T IObservable) =
        let serializer = serializer.Value
        xs |> Observable.map serializer

    static member Deserialize(json) : 'T = deserializer.Value json