[<AutoOpen>]
module TypeInferred.HashBang.Core

open System
open System.Net
open System.Text
open System.Reflection
open System.Globalization
open Microsoft.FSharp.Reflection
open TypeInferred.HashBang.Runtime

type UrlArgument =
    | NoArgument
    | SpecifiedArgument of obj
    | InvalidArgument

type PrimitiveType with
    member pt.RuntimeType =
        match pt with
        | BoolType -> typeof<bool>
        | IntType -> typeof<int>
        | StringType -> typeof<string>
        | UIntType -> typeof<uint32>
        | HexType -> failwith "Hex numbers are not yet supported."
        //| OctalType
        | FloatType -> typeof<float>
        | DecimalType -> typeof<decimal>
        | CharType -> typeof<char>

type UrlPart with

    static member From(template : string) =
        let inside = template.Trim[|'{'; '}'|]
        if inside.Length = template.Length then FixedPart(template.Replace("%%", "%"))
        elif inside.Length = template.Length - 2 then
            match inside.Split [|':'|] with
            | [| name; placeholder |] ->
                let primitiveType =
                    match placeholder with
                    | "%b" -> BoolType
                    | "%d" | "%i" -> IntType
                    | "%s" -> StringType
                    | "%u" -> UIntType
                    | "%x" | "%X" -> HexType
                    //| "%o" -> OctalType |> VariablePart
                    | "%e" | "%E" | "%f" | "%F" | "%g" | "%G" -> FloatType
                    | "%M" -> DecimalType
                    | "%c" -> CharType
                    | _ -> failwithf  "Unsupported placeholder: '%s'." placeholder
                VariablePart(name, primitiveType)
            | _ -> failwithf "Unsupported section format: '%s'." template
        else failwithf "Unsupported section format: '%s'." template

    member part.TryParse(value : string) =
        let inline apply tryParse =
            let x = ref Unchecked.defaultof<_>
            let didParse : bool = tryParse(value, x)
            if didParse then SpecifiedArgument(box x)
            else InvalidArgument
        match part with
        | FixedPart v -> 
            if v = value then NoArgument
            else InvalidArgument
        | VariablePart(_, t) ->
            match t with
            | BoolType -> 
                apply (fun (value, x) -> 
                    Boolean.TryParse(value, x))
            | IntType -> apply Int32.TryParse
            | StringType -> SpecifiedArgument(box value)
            | UIntType -> apply UInt32.TryParse
            | HexType -> 
                apply (fun (value, x) ->
                    UInt32.TryParse(value, NumberStyles.HexNumber, CultureInfo.InvariantCulture, x))
            | FloatType -> apply Single.TryParse
            | DecimalType -> apply Decimal.TryParse
            | CharType -> apply (fun (value, x) -> Char.TryParse(value, x))


type 'a Response =
    /// The request has succeeded.
    | OK of 'a
    /// The server successfully processed the request, but is not returning any content.
    | NoContent
    /// The requested resource has been assigned a new permanent URI.
    | MovedPermanently of Uri
    /// The response to the request can be found under a different URI. ('method -> GET)
    | SeeOther of Uri
    /// The requested resource resides temporarily under a different URI. ('method -> 'method)
    | TemporaryRedirect of Uri
    /// The request could not be understood by the server due to malformed syntax.
    | BadRequest
    /// The request requires user authentication.
    | Unauthorized
    /// The server has not found anything matching the Request-URI.
    | NotFound
    /// The method specified in the Request-Line is not allowed for the resource identified by the Request-URI. 
    | MethodNotAllowed
    /// The server encountered an unexpected condition which prevented it from fulfilling the request.
    | InternalServerError of string

    member response.Code =
        match response with
        | OK _ -> 200
        | NoContent -> 204
        | MovedPermanently _ -> 301
        | SeeOther _ -> 303
        | TemporaryRedirect _ -> 307
        | BadRequest -> 400
        | Unauthorized -> 401
        | NotFound -> 404
        | MethodNotAllowed -> 405
        | InternalServerError _ -> 500

module Response =
    let map f = function
        | OK x -> OK(f x)
        | NoContent -> NoContent
        | MovedPermanently uri -> MovedPermanently uri
        | SeeOther uri -> SeeOther uri
        | TemporaryRedirect uri -> TemporaryRedirect uri
        | BadRequest -> BadRequest
        | Unauthorized -> Unauthorized
        | NotFound -> NotFound
        | MethodNotAllowed -> MethodNotAllowed
        | InternalServerError message -> InternalServerError message

    let bind f = function
        | OK x -> f x
        | NoContent -> NoContent
        | MovedPermanently uri -> MovedPermanently uri
        | SeeOther uri -> SeeOther uri
        | TemporaryRedirect uri -> TemporaryRedirect uri
        | BadRequest -> BadRequest
        | Unauthorized -> Unauthorized
        | NotFound -> NotFound
        | MethodNotAllowed -> MethodNotAllowed
        | InternalServerError message -> InternalServerError message

    let bindAsync f resp =
        async {
            match resp with
            | OK x -> return! f x
            | NoContent -> return NoContent
            | MovedPermanently uri -> return MovedPermanently uri
            | SeeOther uri -> return SeeOther uri
            | TemporaryRedirect uri -> return TemporaryRedirect uri
            | BadRequest -> return BadRequest
            | Unauthorized -> return Unauthorized
            | NotFound -> return NotFound
            | MethodNotAllowed -> return MethodNotAllowed
            | InternalServerError message -> return InternalServerError message
        }

    let fromOptionElse defaultArg = function
        | Some x -> OK x
        | None -> defaultArg

type Path = string[]
type Request = Request of Path * HttpListenerRequest
type HttpMethod = 
    
    Get | Post | Put | Delete

    member meth.HttpName =
        match meth with
        | Get -> "GET"
        | Post -> "POST"
        | Put -> "PUT"
        | Delete -> "DELETE"

type TypeMetadata =
    {
        AssemblyQualifiedName : string
    }
    static member From(t : Type) =
        {
            AssemblyQualifiedName = t.AssemblyQualifiedName
        }

    member meta.RuntimeType =
        let resolveAssembly(name : AssemblyName) = 
            Assembly.Load name
        let resolveType (ass : Assembly) name ignoreCase =
            ass.GetType(name, true, ignoreCase)
        let t =
            Type.GetType(
                meta.AssemblyQualifiedName, 
                (fun name -> resolveAssembly name), 
                (fun ass name ignoreCase -> resolveType ass name ignoreCase),
                true, true)
        match t with
        | null -> failwithf "Could not locate: %s" meta.AssemblyQualifiedName
        | v -> v

type ParameterMetadata =
    {
        Key : string
        Type : PrimitiveType
        IsRequired : bool
    }

type HandlerMetadata =
    {
        Resource : string
        Action : string
        Description : string
        UrlParts : UrlPart[]
        Method : HttpMethod
        Headers : ParameterMetadata[]
        //Parameters : ParameterMetadata[]
        RequestType : (ContentType * TypeMetadata) option
        ResponseType : (ContentType * TypeMetadata) option
    }

    static member Empty =
        {
            Resource = ""
            Action = ""
            Description = ""
            UrlParts = [||]
            Method = Get
            Headers = [||]
            //Parameters = [||]
            RequestType = None
            ResponseType = None
        }

    member meta.Parameters =
        meta.UrlParts |> Array.choose (function
            | FixedPart _ -> None
            | VariablePart(name, t) -> Some { Key = name; Type = t; IsRequired = true }) 

type RequestHandler<'a, 'b> = 
    {
        TryHandle : Request -> ('a -> 'b Response Async) option
        Metadata : HandlerMetadata
    }

module RequestHandler =

    let create (pf:PrintfFormat<_,_,_,_,'t>) (f : 'a -> 't -> 'b Response Async) (meta : HandlerMetadata) =
        let urlTemplate = pf.Value
        let urlParts = 
            urlTemplate.Split([|"/"|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map UrlPart.From
        let cons = 
            let t = typeof<'t>
            if FSharpType.IsTuple t then FSharpValue.PreComputeTupleConstructor t
            elif t = typeof<unit> then fun _ -> () :> obj
            else fun args -> args.[0]
        let meth = meta.Method
        let tryHandle (Request(path, req)) =
            if path.Length = urlParts.Length && req.HttpMethod = meth.HttpName then
                let results = ResizeArray()
                let rec tryPart i =
                    if i >= urlParts.Length then
                        let x = cons(results.ToArray()) :?> 't
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

    
    let chooseIn f (handler : RequestHandler<_,_>) =
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

    let updateMetadata f (handler : RequestHandler<_,_>) =
        { handler with Metadata = f handler.Metadata }

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
//
//    let addQueryParamMetadata param handler =
//        handler |> updateMetadata (fun meta ->
//            {   
//                meta with
//                    QueryParameters = 
//                        [|
//                            yield! meta.QueryParameters
//                            yield param 
//                        |]
//            })

type HandlerSpecification<'a, 't, 'b> =
    {
        Resource : string
        Action : string
        Description : string
        Handle : 'a -> 't -> 'b Response Async
    }

module Json =

    let GET fmt (spec : HandlerSpecification<_,_,_>) = 
        let serialize = precomputeToJson<'f>()
        RequestHandler.create fmt spec.Handle {
            HandlerMetadata.Empty with
                Resource = spec.Resource
                Action = spec.Action
                Description = spec.Description
                Method = Get
                ResponseType = Some(Application Json, TypeMetadata.From typeof<'f>)
        }
        |> RequestHandler.mapOut (Response.map (fun x -> Application Json, serialize x))

    let DELETE fmt (spec : HandlerSpecification<_,_,_>) =
        let serialize = lazy precomputeToJson<'f>()
        RequestHandler.create fmt spec.Handle {
            HandlerMetadata.Empty with
                Resource = spec.Resource
                Action = spec.Action
                Description = spec.Description
                Method = Delete
                //ResponseType = Some(TypeMetadata.From(typeof<'f>, Application Json))
        }
        |> RequestHandler.mapOut (Response.map (fun x -> Application Json, serialize.Value x))

    let POST fmt (spec : HandlerSpecification<_,_,_>) =
        let serialize = precomputeToJson<'f>()
        let deserialize = precomputeFromJson<'g>()
        RequestHandler.create fmt spec.Handle {
            HandlerMetadata.Empty with
                Resource = spec.Resource
                Action = spec.Action
                Description = spec.Description
                Method = Post
                RequestType = Some(Application Json, TypeMetadata.From typeof<'g>)
                ResponseType = Some(Application Json, TypeMetadata.From typeof<'f>)
        }
        |> RequestHandler.chooseIn (fun req y -> async {
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
        |> RequestHandler.mapOut (Response.map (fun x -> Application Json, serialize x))

module PlainText =
    
    let GET fmt (spec : HandlerSpecification<'f,_,_>) =
        RequestHandler.create fmt spec.Handle {
            HandlerMetadata.Empty with
                Resource = spec.Resource
                Action = spec.Action
                Description = spec.Description
                Method = Get
                ResponseType = Some(Text Plain, TypeMetadata.From typeof<'f>)
        }
    //let POST fmt f = createHandler Get fmt f
    //let PUT fmt f = createHandler Get fmt f
    //let DELETE fmt f = createHandler Get fmt f


module Authorization =
    let tryGetToken (req : HttpListenerRequest) =
        match req.Headers.GetValues "Authorization" with
            | [|auth|] -> 
                match auth.Split [|' '|] with
                | [| "HashBang-Auth"; token |] -> Some token
                | _ -> None
            | _ -> None

    let filterByAuthToken (authTokens : TimedCache<_,_>) handler =
        handler |> RequestHandler.chooseIn (fun (req : HttpListenerRequest) () -> async {
            match tryGetToken req with
            | None -> return Unauthorized
            | Some token ->
                match authTokens.TryFind token with
                | Some user -> return OK(req, user)
                | None -> return Unauthorized
        })
        |> RequestHandler.addHeaderMetadata {
            Key = "HashBang-Auth"
            Type = StringType
            IsRequired = true
        }

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
            RedirectTemplate = fun uri -> Text Plain, "Resource is now located at: " + uri.ToString()
            ErrorTemplate = fun msg -> Text Plain, "Error: " + msg
            NotFoundTemplate = Text Plain, "Resource not found."
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
            |]

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
                            let bodyBytes = Encoding.UTF8.GetBytes body
                            rawResp.ContentType <- contentType.Mime
                            rawResp.ContentEncoding <- Text.Encoding.UTF8
                            rawResp.ContentLength64 <- bodyBytes.LongLength
                            let stream = rawResp.OutputStream
                            do! stream.AsyncWrite bodyBytes
                            let! _ = Async.AwaitIAsyncResult(stream.FlushAsync())
                            rawResp.OutputStream.Close()
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