module TypeInferred.HashBang.Routing.Domain

[<ReflectedDefinition>]
type PrimitiveType =
    | BoolType
    | IntType
    | StringType
    | UIntType
    | FloatType
    | DecimalType
    | CharType

    static member Create(typeId : string) =
            match typeId with
            | "bool" -> BoolType
            | "int" -> IntType
            | "string" -> StringType
            | "uint" -> UIntType
            | "float" -> FloatType
            | "decimal" -> DecimalType
            | "char" -> CharType
            | _ -> failwith  ("Unsupported primitive type: " + typeId)


    // Not reflected
    member pt.RuntimeType =
        match pt with
        | BoolType -> typeof<bool>
        | IntType -> typeof<int>
        | StringType -> typeof<string>
        | UIntType -> typeof<uint32>
        | FloatType -> typeof<float>
        | DecimalType -> typeof<decimal>
        | CharType -> typeof<char>

[<ReflectedDefinition>]
type UrlPart =
    | FixedPart of string
    | VariablePart of string * PrimitiveType

    static member Create(template : string) =
        match template.Split [|':'|] |> Array.toList with
        | [name; typeId] -> VariablePart(name, PrimitiveType.Create typeId)
        | [name] -> FixedPart name
        | _ -> failwith ("Unsupported segment format: " + template)

type ParameterMetadata =
    {
        Key : string
        Type : PrimitiveType
        IsRequired : bool
    }

type Path = string[]
type QueryParams = Map<string, string>

type Route =
    {
        UrlFormat : string
        Resource : string
        Action : string
        Description : string
    }

[<ReflectedDefinition>]
type RouteUriBuilder(urlParts : UrlPart[]) =
    let mutable urlParameters = Map.empty<string, string>
    let mutable queryParameters = Map.empty<string, string>

    member __.AddUrlParameter(key, value) =
        urlParameters <- urlParameters.Add(key, value)

    member __.AddQueryParameter(key, value) =
        queryParameters <- queryParameters.Add(key, value)

    member __.Build() =
        let segments = 
            urlParts |> Array.map (fun part ->
                match part with
                | FixedPart section -> section
                | VariablePart(name, _) -> urlParameters.[name])
        WebUtility.CreateUri("/#!", segments, queryParameters)
    
[<ReflectedDefinition>]
type ClientRoute<'ParamT>(tryParse : Path -> QueryParams -> 'ParamT option) =
    member __.CreateHandler(handle : 'ParamT -> 'HtmlT option Async) =
        fun path queryParams -> 
            async {
                match tryParse path queryParams with
                | None -> return None
                | Some ps -> return! handle ps
            }