namespace TypeInferred.HashBang.Owin

open System
open System.IO
open System.Threading.Tasks
open Ionic.Zlib
open Microsoft.Owin
open TypeInferred.HashBang.Owin.Utilities

type CompressionMiddleware(next) =
    inherit OwinMiddleware(next)

    override __.Invoke(context:IOwinContext) : Task =
        async {
            let accepts = context.Request.Headers.GetValues "Accept-Encoding"
            let oldStream = context.Response.Body
            use relayStream = new MemoryStream()
            context.Response.Body <- relayStream
            do! Async.awaitTask(next.Invoke context)
            context.Response.Body <- oldStream
            if relayStream.Length > 0L then
                let uncompressedBytes = relayStream.ToArray()
                let inline hasEncodingSupport encoding = 
                    accepts <> null 
                    && accepts.Contains encoding
                let compressedBytes =
                    if hasEncodingSupport "gzip" then
                        context.Response.Headers.Set("Content-Encoding", "gzip")
                        GZipStream.CompressBuffer uncompressedBytes
                    elif hasEncodingSupport "deflate" then
                        context.Response.Headers.Set("Content-Encoding", "deflate")
                        DeflateStream.CompressBuffer uncompressedBytes
                    else
                        uncompressedBytes
                context.Response.ContentLength <- Nullable(compressedBytes.LongLength)
                return! context.Response.WriteAsync compressedBytes |> Async.awaitTask
        } |> Async.StartAsTask :> Task
    