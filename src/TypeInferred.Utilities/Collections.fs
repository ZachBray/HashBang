[<AutoOpen>]
module CollectionExtensions

open System
open System.Collections.Concurrent
open System.Threading

type Version = int64

type ApproximateMRUCache<'key, 'value>(factory, threshold) =
    let lookup = ConcurrentDictionary<'key, Version * 'value option>()
    let recentHistory = ConcurrentQueue()
    let addFactory = Func<_,_>(fun key -> 0L, factory key)
    let updateFactory = Func<_,_,_>(fun key (version, value) -> version + 1L, value)
    
    let cleanOne() =
        match recentHistory.TryDequeue() with
        | false, _ -> ()
        | true, ((key, editVersion) as edit) ->
            match lookup.TryGetValue(key) with
            | true, (currentVersion, _) when currentVersion = editVersion ->
                if lookup.Count > threshold then lookup.TryRemove(key) |> ignore
                else 
                    // Note: this is the line that makes it approximate!
                    recentHistory.Enqueue edit
            | false, _ | true, _ -> ()

    member __.FindOrCreate key =
        let version, value = lookup.AddOrUpdate(key, addFactory, updateFactory)
        match value with
        | None -> lookup.TryRemove(key) |> ignore
        | Some _ ->
            recentHistory.Enqueue(key, version)
        cleanOne()
        value

    member __.Invalidate key =
        lookup.TryRemove(key) |> ignore

type Time = int64

type TimedCache<'key, 'value>(tickPeriod : TimeSpan, tickLimit) =
    let lookup = ConcurrentDictionary<'key, 'value * Guid>()
    let removals = ConcurrentQueue()
    let mutable tickCount = 0L
    let mutable lastTickTime = DateTime.MinValue
    let mutable nextRemoval = None

    let tryConsumeRemoval() =
        match removals.TryDequeue() with
        | false, _ -> None
        | true, removal -> Some removal

    let rec cleanUp currentTime nextOverride =
        let currentRemoval = 
            match nextOverride with
            | None -> tryConsumeRemoval()
            | x -> x
        match currentRemoval with
        | None -> None
        | Some (expiryTime, key, version) ->
            if expiryTime <= currentTime then
                match lookup.TryGetValue(key) with
                | true, (_, currentVersion) when version = currentVersion ->
                    // Note: There is a race here with AddOrReplace
                    lookup.TryRemove(key) |> ignore
                | true, _ | false, _ -> ()
                cleanUp currentTime None
            else currentRemoval

    let ticker =
        async {
            while true do
                do! Async.Sleep (int tickPeriod.TotalMilliseconds / 2)
                let now = DateTime.UtcNow
                let hasTicked = now - lastTickTime > tickPeriod
                if hasTicked then 
                    let currentTime = Interlocked.Increment &tickCount
                    lastTickTime <- now
                    nextRemoval <- cleanUp currentTime nextRemoval
        } |> Async.StartDisposable

    member __.AddOrReplace key value =
        let version = Guid.NewGuid()
        let value, version = lookup.AddOrUpdate(key, (fun _ -> value, version), (fun _ _ -> value, version))
        let expiryTime = tickCount + tickLimit
        removals.Enqueue(expiryTime, key, version)

    member __.Remove key =
        lookup.TryRemove(key) |> ignore

    member __.TryFind key =
        match lookup.TryGetValue(key) with
        | false, _ -> None
        | true, (v,_) -> Some v

    member __.Dispose() = ticker.Dispose()
    
    interface IDisposable with
        member __.Dispose() = ticker.Dispose()

module Seq =
    let tryHead xs  = Seq.tryFind (fun _ -> true) xs