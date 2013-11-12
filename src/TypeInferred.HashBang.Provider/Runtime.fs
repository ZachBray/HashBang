[<ReflectedDefinition>]
module TypeInferred.HashBang.Provider.Runtime

open System
open System.Net
open System.Text
open System.IO
open System.IO.Compression
open TypeInferred.HashBang.Runtime

module Serialization =

    let mutable serializers = Map.empty<string, obj -> string>
    let mutable deserializers = Map.empty<string, string -> obj>

    let getSerializer(t : Type) =
        match serializers.TryFind t.FullName with
        | Some serialize -> serialize
        | None ->
            let serialize = precomputeTypeToJson t
            serializers <- serializers.Add(t.FullName, serialize)
            serialize

    let getDeserializer(t : Type) =
        match deserializers.TryFind t.FullName with
        | Some deserialize -> deserialize
        | None ->
            let deserialize = precomputeTypeFromJson t
            deserializers <- deserializers.Add(t.FullName, deserialize)
            deserialize

    let serialize t obj = getSerializer t obj
    let deserialize t obj = getDeserializer t obj
        

type ApiDataContext =
    {
        BaseUrl : string
    }

type ApiRequest(httpMethod, urlParts : UrlPart[]) =
    let mutable headers = Map.empty<string, string>
    let mutable urlParameters = Map.empty<string, string>
    let mutable queryParameters = Map.empty<string, string>
    let mutable body = None

    member __.AddHeader(key, value) = 
        headers <- headers.Add(key, value)

    member __.AddUrlParameter(key, value) =
        urlParameters <- urlParameters.Add(key, value)

    member __.AddQueryParameter(key, value) =
        queryParameters <- queryParameters.Add(key, value)

    member __.AddBody v = body <- Some v

    member __.Headers = headers

    member __.BuildUrl(dc : ApiDataContext) =
        let segments = 
            urlParts |> Array.map (fun part ->
                match part with
                | FixedPart section -> section
                | VariablePart(name, _) -> urlParameters.[name])
        System.Net.WebUtility.CreateUri(dc.BaseUrl, segments, queryParameters)
    
    member req.Send(dc) =
        async {
            let httpReq = WebRequest.Create(req.BuildUrl dc)
            httpReq.Method <- httpMethod
            httpReq.Headers.Add("Accept", "application/json")
            httpReq.Headers.Add("Accept-Encoding", "gzip")
            // This causes an additional OPTIONS request...
            // httpReq.Headers.Add("Content-Type", "application/json")
            headers |> Map.iter (fun key value ->
                httpReq.Headers.Add(key, value))
            match body with
            | None -> ()
            | Some(bodyText : string) ->
                use input = httpReq.GetRequestStream()
                let inBytes = Encoding.UTF8.GetBytes bodyText
                do! input.AsyncWrite inBytes
            use! httpRsp = httpReq.AsyncGetResponse()
            if httpRsp.ContentLength > 0L then
                use outputStream = httpRsp.GetResponseStream()
                let! bytes = outputStream.AsyncRead(int httpRsp.ContentLength)
                use memoryStream = new MemoryStream()
                use uncompressedStream = new GZipStream(memoryStream, CompressionMode.Decompress, false)
                uncompressedStream.Write(bytes, 0, bytes.Length)
                uncompressedStream.Flush()
                uncompressedStream.Close()
                let outBytes = memoryStream.ToArray()
                let outText = Encoding.UTF8.GetString outBytes
                return Some outText
            else
                return None
        }


    member req.SendIgnore(dc) =
        async {
            let! _ = req.Send(dc)
            return ()
        }

    member req.SendAndDeserialize(t, dc) =
        async { 
            let! result = req.Send(dc)
            match result with
            | None -> return failwith "The response had no content."
            | Some json -> return Serialization.deserialize t json
        }