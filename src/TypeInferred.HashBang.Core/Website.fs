[<AutoOpen>]
module TypeInferred.HashBang.Core

open System
open System.Net
open System.IO
open System.IO.Compression
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

    static member Start (site : Website) =
        
        let getResponse (Request(_,_,raw) as request) = async {
        // TODO: Could speed this up by building a radix tree of the UrlParts.
            try
                let result = 
                    site.Handlers |> List.tryPick (fun (handler : RequestHandler<_,_>) -> 
                        if raw.HttpMethod = handler.Metadata.Method.HttpName then
                            handler.TryHandle request |> Option.map (fun f -> f())
                        else None)
                return! defaultArgAsync result NotFound
            with ex ->
                return InternalServerError(ex.ToString())
        }

        let defaultHeaders =
            [|  "Cache-Control", "no-cache, no-store, must-revalidate"
                "Pragma", "no-cache"
                "Expires", "0"
                "Access-Control-Allow-Origin", "*"
                "Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept, Authorization, Accept-Encoding"
            |]

        // TODO: Do negotiations... content-type, encoding, etc.
        HttpListener.create site.Prefix (fun rawReq rawResp ->
            async {
                try
                    try
                        let segments, queryParams = System.Net.WebUtility.SplitRelativeUri rawReq.RawUrl
                        let request = Request(segments, queryParams, rawReq)
                        let! response = getResponse request
                        rawResp.StatusCode <- response.Code
                        defaultHeaders |> Array.iter rawResp.AddHeader
                        let content =
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
                        match content with
                        | None -> ()
                        | Some(contentType, uncompressed) ->
                            let bodyBytes = 
                                match rawReq.Headers.["Accept-Encoding"] with
                                | vs when vs <> null && vs.Contains "gzip" -> 
                                    rawResp.AddHeader("Content-Encoding", "gzip")
                                    use memoryStream = new MemoryStream()
                                    use compressor = new GZipStream(memoryStream, CompressionMode.Compress, false)
                                    compressor.Write(uncompressed, 0, uncompressed.Length)
                                    compressor.Flush()
                                    compressor.Close()
                                    memoryStream.ToArray()
                                | _ -> 
                                    rawResp.ContentEncoding <- Encoding.UTF8
                                    uncompressed                                
                            rawResp.ContentLength64 <- bodyBytes.LongLength 
                            rawResp.ContentType <- contentType.Mime                           
                            use stream = rawResp.OutputStream
                            do! stream.AsyncWrite bodyBytes
                            let! _ = Async.AwaitIAsyncResult(stream.FlushAsync())
                            stream.Close()
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