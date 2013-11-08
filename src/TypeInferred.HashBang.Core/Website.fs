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
        Handlers : RequestHandler<unit, ContentType * string> list
        RedirectTemplate : Uri -> ContentType * string
        ErrorTemplate : string -> ContentType * string
        NotFoundTemplate : ContentType * string
    }

    static member At prefix =
        {
            Prefix = prefix
            Handlers = []
            RedirectTemplate = fun uri -> ContentTypes.Text.plain, "Resource is now located at: " + uri.ToString()
            ErrorTemplate = fun msg -> ContentTypes.Text.plain, "Error: " + msg
            NotFoundTemplate = ContentTypes.Text.plain, "Resource not found."
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
            Json.GET "/metadata" {
                Resource = "Metadata"; Action = "Get"
                Description = "Returns website metadata."
                Handle = fun req () -> async {
                    return OK handlersMetadata
                }
            }
        ]

    static member Start (site : Website) =
        
        let getResponse request = async {
        // TODO: Could speed this up by building a radix tree of the UrlParts.
            try
                let result = 
                    site.Handlers |> List.tryPick (fun (handler : RequestHandler<_,_>) -> 
                        handler.TryHandle request |> Option.map (fun f -> f()))
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
                        let request = 
                            let path = rawReq.RawUrl.Split([|'/'|], StringSplitOptions.RemoveEmptyEntries)
                            Request(path, rawReq)
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
                        | Some(contentType, body) ->
                            let bodyBytes = 
                                let uncompressed = Encoding.UTF8.GetBytes body
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
                                    rawResp.ContentEncoding <- Text.Encoding.UTF8
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