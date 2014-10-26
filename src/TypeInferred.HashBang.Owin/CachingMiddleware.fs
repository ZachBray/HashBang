namespace TypeInferred.HashBang.Owin

open System
open System.IO
open System.Threading.Tasks
open System.Collections.Concurrent
open Microsoft.Owin
open TypeInferred.HashBang.Owin.Utilities
open TypeInferred.HashBang

type CachingMiddleware(next) =
    inherit OwinMiddleware(next)

    let getKey (request:IOwinRequest) =
        match request.Accept, request.Headers.GetCommaSeparatedValues("Accept-Encoding") with
        | null, _ | _, null -> None
        | accept, acceptEncoding -> Some(request.Uri.ToString(), accept, acceptEncoding)
        
    let cache = ConcurrentDictionary(HashIdentity.Structural)

    override __.Invoke(context:IOwinContext) : Task =
        async {
            let key = getKey context.Request
            match key with
            | None -> return! Async.awaitTask(next.Invoke context)
            | Some requestKey ->
                match cache.TryGetValue requestKey with
                | true, (headers, payload:byte[]) ->
                    headers |> Array.iter (fun (key, values) -> context.Response.Headers.SetValues(key, values))
                    return! context.Response.WriteAsync payload |> Async.awaitTask
                | false, _ ->
                    let oldStream = context.Response.Body
                    use relayStream = new MemoryStream()
                    context.Response.Body <- relayStream
                    do! Async.awaitTask(next.Invoke context)
                    context.Response.Body <- oldStream
                    if relayStream.Length > 0L then
                        let payload = relayStream.ToArray()
                        let headers =
                            context.Response.Headers |> Seq.map (fun header ->
                                header.Key, header.Value) |> Seq.toArray
                        cache.TryAdd(requestKey, (headers, payload)) |> ignore
                        return! context.Response.WriteAsync payload |> Async.awaitTask
        } |> Async.StartAsTask :> Task
    