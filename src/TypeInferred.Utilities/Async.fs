[<AutoOpen>]
module AsyncExtensions

open System
open System.Threading

type Async with
    static member StartDisposable task =
        let cts = new CancellationTokenSource()
        Async.Start(task, cts.Token)
        Disposable.by (fun () ->
            cts.Cancel()
            cts.Dispose())
        
    static member map f xAsync =
        async {
            let! x = xAsync
            return f x
        }

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