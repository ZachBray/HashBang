namespace TypeInferred.HashBang

open System.Text

open TypeInferred.HashBang.Runtime

type HandlerSpecification<'a, 't, 'b> =
    {
        Resource : string
        Action : string
        Description : string
        Handle : 'a -> 't -> 'b Response Async
    }

    member spec.ToMetadata() =
        {   HandlerMetadata.Empty with
                Resource = spec.Resource
                Action = spec.Action
                Description = spec.Description
        }

module Json =

    let GET fmt (spec : HandlerSpecification<_,_,_>) = 
        let serialize = precomputeToJson<'f>()
        RequestHandler.create fmt spec.Handle {
            spec.ToMetadata() with
                Method = Get
                ResponseType = Some(ContentTypes.Application.json, TypeMetadata.From typeof<'f>)
        }
        |> RequestHandler.mapOut (Response.map (fun x -> ContentTypes.Application.json, serialize x))

    let DELETE fmt (spec : HandlerSpecification<_,_,_>) =
        let serialize = lazy precomputeToJson<'f>()
        RequestHandler.create fmt spec.Handle {
            spec.ToMetadata() with
                Method = Delete
                //ResponseType = Some(TypeMetadata.From(typeof<'f>, Application Json))
        }
        |> RequestHandler.mapOut (Response.map (fun x -> ContentTypes.Application.json, serialize.Value x))

    let POST fmt (spec : HandlerSpecification<_,_,_>) =
        let serialize = precomputeToJson<'f>()
        let deserialize = precomputeFromJson<'g>()
        RequestHandler.create fmt spec.Handle {
            spec.ToMetadata() with
                Method = Post
                RequestType = Some(ContentTypes.Application.json, TypeMetadata.From typeof<'g>)
                ResponseType = Some(ContentTypes.Application.json, TypeMetadata.From typeof<'f>)
        }
        |> RequestHandler.bindIn (fun req y -> async {
            try
                // Assumption: Stream length is under int32 limit.
                let! jsonBytes = req.InputStream.AsyncRead(int req.ContentLength64)
                let json = Encoding.UTF8.GetString jsonBytes
                return OK(deserialize json, y)
            with ex ->
                //TODO: log
                printfn "%s" (ex.ToString())
                return BadRequest
        })
        |> RequestHandler.mapOut (Response.map (fun x -> ContentTypes.Application.json, serialize x))

module PlainText =
    
    let GET fmt (spec : HandlerSpecification<_,_,string>) =
        RequestHandler.create fmt spec.Handle {
            spec.ToMetadata() with
                Method = Get
                ResponseType = Some(ContentTypes.Text.plain, TypeMetadata.From typeof<string>)
        }

module Binary =
    
    let GET fmt (spec : HandlerSpecification<_,_,byte[]>) =
        RequestHandler.create fmt spec.Handle {
            spec.ToMetadata() with
                Method = Get
                ResponseType = Some(ContentTypes.Application.octet_stream, TypeMetadata.From typeof<byte[]>)
        }