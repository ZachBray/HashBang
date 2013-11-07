namespace TypeInferred.HashBang

open System
open System.Net
open Microsoft.FSharp.Reflection

open TypeInferred.HashBang.Runtime

type Path = string[]
type Request = Request of Path * HttpListenerRequest

type RequestHandler<'parentParamTuple, 'contentType> = 
    {
        TryHandle : Request -> ('parentParamTuple -> 'contentType Response Async) option
        Metadata : HandlerMetadata
    }

module RequestHandler =

    let create (pf:PrintfFormat<_,_,_,_,'routeParamTuple>) 
               (f : 'parentParamTuple -> 'routeParamTuple -> 'contentType Response Async) 
               (meta : HandlerMetadata) =
        let urlTemplate = pf.Value
        let urlParts = 
            urlTemplate.Split([|"/"|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map UrlPart.From
        let cons = 
            let t = typeof<'routeParamTuple>
            if FSharpType.IsTuple t then FSharpValue.PreComputeTupleConstructor t
            elif t = typeof<unit> then fun _ -> () :> obj
            else fun args -> args.[0]
        let meth = meta.Method
        let tryHandle (Request(path, req)) =
            if path.Length = urlParts.Length && req.HttpMethod = meth.HttpName then
                let results = ResizeArray()
                let rec tryPart i =
                    if i >= urlParts.Length then
                        let x = cons(results.ToArray()) :?> 'routeParamTuple
                        (fun y -> f y x) |> Some
                    else
                        match urlParts.[i].TryParse path.[i] with
                        | NoArgument -> tryPart (i+1)
                        | SpecifiedArgument obj -> 
                            results.Add obj
                            tryPart (i+1)
                        | InvalidArgument -> None
                tryPart 0
            else None
        {
            TryHandle = tryHandle
            Metadata = { meta with UrlParts = urlParts }
        }

    /// Maps the response (content) type using f
    let mapOut f (handler : RequestHandler<_,_>) =
        {   
            TryHandle = fun request ->
                match handler.TryHandle request with
                | None -> None
                | Some g ->
                    Some(fun y ->
                        async {
                            let! x = g y
                            return f x
                        })
            Metadata = handler.Metadata
        }

    /// Maps the input (parameter) type using f
    let mapIn f (handler : RequestHandler<_,_>) =
        {   
            TryHandle = fun (Request(_, raw) as request) ->
                match handler.TryHandle request with
                | None -> None
                | Some g -> Some(fun y -> 
                    async {
                        let! z = f raw y
                        return! g z
                    })
            Metadata = handler.Metadata
        }

    
    /// Attempts to map the input (parameter) type using f.
    /// If f returns "OK" this is passed to the next stage.
    /// Otherwise, the error code / redirect is returned immediately.
    let bindIn f (handler : RequestHandler<_,_>) =
        {   
            TryHandle = fun (Request(_, raw) as request) ->
                match handler.TryHandle request with
                | None -> None
                | Some g -> Some(fun y -> 
                    async {
                        let! z = f raw y 
                        return! z |> Response.bindAsync g
                    })
            Metadata = handler.Metadata
        }

    /// Maps the metadata for the handler.
    let updateMetadata f (handler : RequestHandler<_,_>) =
        { handler with Metadata = f handler.Metadata }

    /// Appends parameter metadata to the handler
    let addHeaderMetadata param handler =
        handler |> updateMetadata (fun meta ->
            {   
                meta with
                    Headers = 
                        [|
                            yield! meta.Headers
                            yield param 
                        |]
            })