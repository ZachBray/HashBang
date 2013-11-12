[<AutoOpen>]
module AsyncExtensions

open System
open System.Threading

[<ReflectedDefinition>]
module Async =
    let StartDisposable task =
        let cts = new CancellationTokenSource()
        Async.Start(task, cts.Token)
        Disposable.by (fun () ->
            cts.Cancel()
            cts.Dispose())
        
    let map f xAsync =
        async {
            let! x = xAsync
            return f x
        }

[<ReflectedDefinition>]
module List =
    let tryPickAsync f xs =
        let rec tryPickAsync = function
            | [] -> async { return None }
            | x::xs ->
                async {
                    let! y = f x
                    match y with
                    | None -> return! tryPickAsync xs
                    | Some _ -> return y
                }
        tryPickAsync xs

    let foldAsync f seed xs =
        let rec foldAsync acc = function
            | [] -> async { return acc }
            | x::xs -> 
                async { 
                    let! acc = f acc x
                    return! foldAsync acc xs
                }
        foldAsync seed xs
            