namespace TypeInferred.HashBang

open System
open System.Net
open Microsoft.FSharp.Reflection

open TypeInferred.HashBang.Runtime

type Path = string[]
type QueryParams = Map<string, string>
type Request = Request of Path * QueryParams * HttpListenerRequest

type RequestHandler<'ParentParamsT, 'ResponseT> = 
    {
        TryHandle : Request -> ('ParentParamsT -> 'ResponseT Response Async) option
        Metadata : HandlerMetadata
    }

type RequestHandlerFactory<'LocalParamsT>
        (metadata : HandlerMetadata, 
         parseRequest : Path -> QueryParams -> 'LocalParamsT option) =
    /// Defines a request handler.
    /// 'ParentParamT is the type of any parameters coming from outer layers 
    /// (e.g., authorization may add a user parameter).
    /// 'LocalParamsT is the type of the parameters parsed from the route itself.
    /// 'ContentT is the type of the resulting response (note: it is likely
    /// that this will be mapped by an outer layer of middleware, e.g., to JSON).
    member __.Create (handle : 'ParentParamT -> 'LocalParamsT -> 'ResponseT Response Async) =
        {
            TryHandle = fun (Request(path, queryParams, _)) ->
                match parseRequest path queryParams with
                | None -> None
                | Some localParams -> 
                    Some(fun parentParams -> handle parentParams localParams)
            Metadata = metadata
        }

module RequestHandler =
    
    /// Creates a static request handler that doesn't have any route parameters.
    let createStatic (urlPath : string) spec =
        let parts = urlPath.Split([|'/'|], StringSplitOptions.RemoveEmptyEntries)
        let parseRequest urlParts _ =
             if parts = urlParts then Some()
             else None
        let metadata =
            { HandlerMetadata.Empty with
                UrlParts = parts |> Array.map FixedPart
            }
        RequestHandlerFactory(metadata, parseRequest).Create spec

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
            TryHandle = fun request ->
                match handler.TryHandle request with
                | None -> None
                | Some g -> Some(fun y -> 
                    async {
                        let! z = f request y
                        return! g z
                    })
            Metadata = handler.Metadata
        }

    /// Attempts to map the input (parameter) type using f.
    /// If f returns "OK" this is passed to the next stage.
    /// Otherwise, the error code / redirect is returned immediately.
    let bindIn f (handler : RequestHandler<_,_>) =
        {   
            TryHandle = fun request ->
                match handler.TryHandle request with
                | None -> None
                | Some g -> Some(fun y -> 
                    async {
                        let! z = f request y 
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
    /// Appends parameter metadata to the handler
    let addQueryParameterMetadata param handler =
        handler |> updateMetadata (fun meta ->
            {   
                meta with
                    QueryParameters = 
                        [|
                            yield! meta.QueryParameters
                            yield param 
                        |]
            })

[<AutoOpen>]
module Operators =
    let (--|) handlerMapping route = handlerMapping, route
    let (|-->) (handlerMapping, route) handlerSpec = 
        RequestHandler.createStatic route handlerSpec
        |> handlerMapping

    let GET (serializerFactory : unit -> ContentTypeSerializer<'ResponseType>) 
        (handler : RequestHandler<'ParamType, 'ResponseType>) =
        let serializer = serializerFactory()
        handler
        |> RequestHandler.mapOut (Response.map (fun x -> serializer.ContentType, serializer.Serialize x))
        |> RequestHandler.updateMetadata (fun metadata ->
            { metadata with 
                Method = Get
                ResponseType = 
                    Some(serializer.ContentType, 
                         TypeMetadata.From typeof<'ResponseType>) })

    let DELETE serializerFactory handler =
        GET serializerFactory handler
        |> RequestHandler.updateMetadata (fun metadata ->
            { metadata with Method = Delete })

    let POST (deserializerFactory : unit -> ContentTypeSerializer<'PostType>) 
             (serializerFactory : unit -> ContentTypeSerializer<'ResponseType>) 
             (handler : RequestHandler<'ParamType * 'PostType, 'ResponseType>) =
        let deserializer = deserializerFactory()
        GET serializerFactory handler
        |> RequestHandler.bindIn (fun (Request(_,_,req)) y -> async {
            try
                // Assumption: Stream length is under int32 limit.
                let! jsonBytes = req.InputStream.AsyncRead(int req.ContentLength64)
                return OK(y, deserializer.Deserialize jsonBytes)
            with ex ->
                //TODO: log
                printfn "%s" (ex.ToString())
                return BadRequest
        })
        |> RequestHandler.updateMetadata (fun metadata ->
            { metadata with 
                Method = Post
                RequestType = 
                    Some(deserializer.ContentType, 
                         TypeMetadata.From typeof<'PostType>) })

    let PUT deserializerFactory serializerFactory handler =
        POST deserializerFactory serializerFactory handler
        |> RequestHandler.updateMetadata (fun metadata ->
            { metadata with Method = Put })

type GetRequestHandlerFactory<'LocalParamsT>(metadata, parseRequest) =
    let handlerFactory = RequestHandlerFactory<'LocalParamsT>(metadata, parseRequest)
    member __.CreateHandler serializer handle =
        handlerFactory.Create handle
        |> GET serializer

type DeleteRequestHandlerFactory<'LocalParamsT>(metadata, parseRequest) =
    let handlerFactory = RequestHandlerFactory<'LocalParamsT>(metadata, parseRequest)
    member __.CreateHandler serializer handle =
        handlerFactory.Create handle
        |> DELETE serializer

type PostRequestHandlerFactory<'LocalParamsT>(metadata, parseRequest) =
    let handlerFactory = RequestHandlerFactory<'LocalParamsT>(metadata, parseRequest)
    member __.CreateHandler deserializer serializer handle =
        handlerFactory.Create handle
        |> POST deserializer serializer

type PutRequestHandlerFactory<'LocalParamsT>(metadata, parseRequest) =
    let handlerFactory = RequestHandlerFactory<'LocalParamsT>(metadata, parseRequest)
    member __.CreateHandler deserializer serializer handle =
        handlerFactory.Create handle
        |> PUT deserializer serializer