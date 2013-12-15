[<AutoOpen>]
module TypeInferred.HashBang.Core

open System
open System.Net
open System.IO
open Ionic.Zlib
//open System.IO.Compression
open System.Text
open TypeInferred.HashBang.Runtime

type Website =
    {
        Prefix : string
        Handlers : RequestHandler<unit, ContentType * byte[]> list
        RedirectTemplate : Uri -> ContentType * byte[]
        ErrorTemplate : string -> ContentType * byte[]
        NotFoundTemplate : ContentType * byte[]
    }

    static member At prefix =
        {
            Prefix = prefix
            Handlers = []
            RedirectTemplate = fun uri -> 
                ContentTypes.Text.plain, 
                Encoding.UTF8.GetBytes("Resource is now located at: " + uri.ToString())
            ErrorTemplate = fun msg -> 
                ContentTypes.Text.plain, 
                 Encoding.UTF8.GetBytes("Error: " + msg)
            NotFoundTemplate = ContentTypes.Text.plain, Encoding.UTF8.GetBytes "Resource not found."
        }

    static member WithRouteHandlers newHandlers (site : Website) =
        {   site with
                Handlers = site.Handlers @ newHandlers
        }

    static member WithMetadata site =
        let handlersMetadata = 
            site.Handlers 
            |> List.map (fun handler -> handler.Metadata) 
            |> List.toArray
        site |> Website.WithRouteHandlers [
            GET Json --|"/metadata"|--> fun () () -> async {
                return OK handlersMetadata
            }
        ]

    static member private GetContent(site : Website, rawResp : HttpListenerResponse, response) =
        match response with
        | OK(contentType, body) -> Some(contentType, body)
        | NoContent -> None
        | TemporaryRedirect url | SeeOther url | MovedPermanently url ->
            rawResp.RedirectLocation <- url.ToString()
            site.RedirectTemplate url |> Some
        | BadRequest -> site.ErrorTemplate "Bad request." |> Some
        | InternalServerError message -> site.ErrorTemplate message |> Some
        | MethodNotAllowed -> site.ErrorTemplate "Method not allowed." |> Some
        | NotFound -> site.NotFoundTemplate |> Some
        | Unauthorized -> 
            rawResp.AddHeader("WWW-Authenticate", "HashBang-Auth")
            site.ErrorTemplate "Unauthorized access." |> Some

    static member private GetResponse(site : Website, (Request(_,_,raw) as request)) =
        async {
            // TODO: Could speed this up by building a radix tree of the UrlParts.
            try
                let result = 
                    site.Handlers |> List.tryPick (fun (handler : RequestHandler<_,_>) -> 
                        if raw.HttpMethod = handler.Metadata.Method.HttpName then
                            handler.TryHandle request |> Option.map (fun f -> 
                                f() |> Async.map (fun x -> handler.Metadata, x))
                        else None)
                return! defaultArgAsync result (HandlerMetadata.Empty, NotFound)
            with ex ->
                return HandlerMetadata.Empty, InternalServerError(ex.ToString())
        }

    static member private GetResponseBytes(rawResp : HttpListenerResponse, uncompressed : byte[], compressionLevel) =
        match compressionLevel with
        | Some(level : CompressionLevel) ->
            rawResp.AddHeader("Content-Encoding", "gzip")
            use memoryStream = new MemoryStream()
            use compressor = new GZipStream(memoryStream, CompressionMode.Compress, level, false)
            compressor.Write(uncompressed, 0, uncompressed.Length)
            compressor.Flush()
            compressor.Close()
            memoryStream.ToArray()
        | None ->
            rawResp.ContentEncoding <- Encoding.UTF8
            uncompressed    

    static member SendBytes(rawResp : HttpListenerResponse, payload : byte[], contentType : ContentType) =
        async {
            rawResp.ContentLength64 <- payload.LongLength
            rawResp.ContentType <- contentType.Mime
            use stream = rawResp.OutputStream
            do! stream.AsyncWrite payload
            stream.Flush()
            stream.Close()
        }

    static member Start (site : Website) =

        let defaultHeaders =
            [|  "Cache-Control", "no-cache, no-store, must-revalidate"
                "Pragma", "no-cache"
                "Expires", "0"
                "Access-Control-Allow-Origin", "*"
                "Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept, Authorization, Accept-Encoding"
            |]

        let cache = System.Collections.Concurrent.ConcurrentDictionary(HashIdentity.Structural)

        // TODO: Do negotiations... content-type, encoding, etc.
        HttpListener.create site.Prefix (fun rawReq rawResp ->
            async {
                try
                    try
                        let segments, queryParams = System.Net.WebUtility.SplitRelativeUri rawReq.RawUrl
                        let request = Request(segments, queryParams, rawReq)
                        let acceptsGZip =
                            let vs = rawReq.Headers.["Accept-Encoding"]
                            vs <> null && vs.Contains "gzip"
                        let requestKey = segments, queryParams, acceptsGZip
                        match cache.TryGetValue requestKey with
                        | true, (headers, payload, contentType) ->
                            headers |> Array.iter rawResp.AddHeader
                            do! Website.SendBytes(rawResp, payload, contentType)
                        | false, _ ->
                            let! metadata, response = Website.GetResponse(site, request)
                            rawResp.StatusCode <- response.Code
                            defaultHeaders |> Array.iter rawResp.AddHeader
                            let content = Website.GetContent(site, rawResp, response)
                            match content with
                            | None -> ()
                            | Some(contentType, uncompressed) ->
                                let compressionLevel =
                                    if acceptsGZip then
                                        if metadata.CanCacheResponse then Some CompressionLevel.BestCompression
                                        else Some CompressionLevel.BestSpeed
                                    else None
                                let payload = Website.GetResponseBytes(rawResp, uncompressed, compressionLevel)
                                do! Website.SendBytes(rawResp, payload, contentType)
                                if metadata.CanCacheResponse then
                                    let headers = rawResp.Headers.AllKeys |> Array.map (fun k -> k, rawResp.Headers.[k])
                                    cache.TryAdd(requestKey, (headers, payload, contentType)) |> ignore
                    with ex ->
                        printfn "%s" (ex.ToString())
                        //TODO: log
                finally
                    try rawResp.Close()
                    with ex -> 
                        printfn "%s" (ex.ToString())
                        //TODO: log
            }
        )