module TypeInferred.HashBang.Owin.Utilities

open System
open System.Threading

module List =
    [<ReflectedDefinition>]
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

module Async =
    [<ReflectedDefinition>]
    let map f x =
        async {
            let! y = x
            return f y
        }

    let cache x =
        let cachingAgent = MailboxProcessor<AsyncReplyChannel<_>>.Start(fun inbox -> async {
            let! channel = inbox.Receive()
            try
                let! result = x
                channel.Reply(fun () -> result)
                while true do
                    let! channel = inbox.Receive()
                    channel.Reply(fun () -> result)
            with ex ->
                channel.Reply(fun () -> raise ex)
                while true do
                    let! repl = inbox.Receive()
                    channel.Reply(fun () -> raise ex)})
        cachingAgent.PostAndAsyncReply id
        |> map (fun f -> f())

    
    let awaitTask x = x |> Async.AwaitIAsyncResult |> Async.Ignore

[<AutoOpen>]
module internal Stopwatch =
    let inline time description f = 
        let sw = Diagnostics.Stopwatch.StartNew()
        let r = f()
        sw.Stop()
        Console.WriteLine(sprintf "%s took %0.0fms" description sw.Elapsed.TotalMilliseconds)
        r