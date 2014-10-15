namespace TypeInferred.HashBang.SignalR

open System.Collections.Concurrent

type internal Cache<'TKey, 'TValue>(factory) =
    let items = ConcurrentDictionary()

    member __.FindOrCreate(key : 'TKey) : 'TValue =
        match items.TryGetValue(key) with
        | false, _ -> 
            let newValue = lazy factory key
            if items.TryAdd(key, newValue) then newValue.Value
            else items.[key].Value
        | true, v -> v.Value

    member __.TryRemove(key) =
        let mutable removed = Unchecked.defaultof<_>
        if items.TryRemove(key, &removed) && removed.IsValueCreated then
            Some removed.Value
        else None