namespace TypeInferred.HashBang.Routing

open System
open System.Reflection
open System.IO
open System.Net
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open Samples.FSharp.ProvidedTypes
open TypeInferred.HashBang
open Runtime

type Route =
    {
        Method : HttpMethod
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
        System.Net.WebUtility.CreateUri("/#!", segments, queryParameters)
    
[<ReflectedDefinition>]
type ClientRoute<'ParamT>(tryParse : Path -> QueryParams -> 'ParamT option) =
    member __.CreateHandler(handle : 'ParamT -> 'HtmlT option Async) =
        fun path queryParams -> 
            async {
                match tryParse path queryParams with
                | None -> return None
                | Some ps -> return! handle ps
            }
    //member __.CreateCall


module Generation =
    
    let parseRoute (routeFormat : string) =
        let fail() = 
            failwithf "Expected '(GET|POST|PUT|DELETE) <segments> # <resource>.<action> # <description>' but got '%s'" routeFormat
        match routeFormat.Split [|'#'|] with
        | [|route; resourcePath; doc|] ->
            match route.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) with
            | [|httpMethod; path|] ->
                match resourcePath.Trim().Split [|'.'|] with
                | [|resource; action|] ->
                    let httpMethod =
                        match httpMethod.Trim() with
                        | "GET" -> Get
                        | "POST" -> Post
                        | "PUT" -> Put
                        | "DELETE" -> Delete
                        | _ -> failwith "Unsupported HTTP method."
                    {
                        Method = httpMethod
                        UrlFormat = path
                        Resource = resource
                        Action = action
                        Description = doc
                    }
                | _ -> fail()
            | _ -> fail()
        | _ -> fail()

    let parseClientRoute (routeFormat : string) =
        let fail() = 
            failwithf "Expected '<segments> # <resource>.<action> # <description>' but got '%s'" routeFormat
        match routeFormat.Split [|'#'|] with
        | [|route; resourcePath; doc|] ->
            match route.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) with
            | [|path|] ->
                match resourcePath.Trim().Split [|'.'|] with
                | [|resource; action|] ->
                    {
                        Method = Get
                        UrlFormat = path
                        Resource = resource
                        Action = action
                        Description = doc
                    }
                | _ -> fail()
            | _ -> fail()
        | _ -> fail()

    let generateMetadata (route : Route) =
        let segments, querySpecs = WebUtility.SplitRelativeUri route.UrlFormat
        let urlParts = segments |> Array.map (fun seg -> UrlPart.Create seg, <@@ UrlPart.Create seg @@>)
        let urlPartsExpr =
            let elementExprs = urlParts |> Array.map snd |> Array.toList
            Expr.NewArray(typeof<UrlPart>, elementExprs)
        let queryParams = querySpecs |> Map.map (fun k v -> 
            let str, isRequired =
                if v.EndsWith("_option") then 
                    v.Substring(0, v.Length - "_option".Length), false
                else v, true
            { Key = k; Type = PrimitiveType.Create str; IsRequired = isRequired },
            <@@ { Key = k; Type = PrimitiveType.Create str; IsRequired = isRequired } @@>)
        let queryParamsExpr =
            let elementExprs = queryParams |> Map.toSeq |> Seq.map (snd >> snd) |> Seq.toList
            Expr.NewArray(typeof<ParameterMetadata>, elementExprs)
        let resource = route.Resource
        let action = route.Action
        let description = route.Description
        let metadataExpr =
            <@
                { HandlerMetadata.Empty with 
                    UrlParts = %%urlPartsExpr
                    QueryParameters = %%queryParamsExpr
                    Resource = resource
                    Action = action
                    Description = description
                }
            @>
        urlParts, queryParams, metadataExpr

    let buildCheckAccess t (element : Expr<string>) =
        match t with
        | BoolType -> 
            <@ Boolean.TryConvert(%element).HasValue @>, 
            Some <@@ Boolean.Parse(%element) @@>
        | UIntType -> 
            <@ UInt32.TryConvert(%element).HasValue @>,
            Some <@@ UInt32.Parse(%element) @@>
        | IntType -> 
            <@ Int32.TryConvert(%element).HasValue @>,
            Some <@@ Int32.Parse(%element) @@>
        | CharType ->
            <@ Char.TryConvert(%element).HasValue @>,
            Some <@@ Char.Parse(%element) @@>
        | DecimalType ->
            <@ Decimal.TryConvert(%element).HasValue @>,
            Some <@@ Decimal.Parse(%element) @@>
        | FloatType ->
            <@ Double.TryConvert(%element).HasValue @>,
            Some <@@ Double.Parse(%element) @@>
        | StringType ->
            <@ true @>,
            Some <@@ %element : string @@>

    let buildTryConvert t (element : Expr<string option>) =
        match t with
        | BoolType -> 
            <@@ %element |> Option.bindNullable Boolean.TryConvert @@>
        | UIntType -> 
            <@@ %element |> Option.bindNullable UInt32.TryConvert @@>
        | IntType -> 
            <@@ %element |> Option.bindNullable Int32.TryConvert @@>
        | CharType ->
            <@@ %element |> Option.bindNullable Char.TryConvert @@>
        | DecimalType ->
            <@@ %element |> Option.bindNullable Decimal.TryConvert @@>
        | FloatType ->
            <@@ %element |> Option.bindNullable Double.TryConvert @@>
        | StringType -> <@@ %element @@>

    let genericNullableDef = typedefof<_ Nullable>

    let generateTypeAndFactory (route : Route) urlParts queryParams =
        let urlElements = 
            urlParts |> Seq.choose (fst >> (function
                | VariablePart(name, t) -> Some(name, t, true)
                | FixedPart _ -> None))
        let queryElements =
            queryParams |> Map.toSeq |> Seq.map (snd >> fst)
            |> Seq.map (fun p -> p.Key, p.Type, p.IsRequired)
        let elements = 
            Seq.append urlElements queryElements
            |> Seq.map (fun (name, t, isReq) -> 
                name,
                match t, isReq with
                | StringType, false -> typeof<string option>
                | _, false -> genericNullableDef.MakeGenericType t.RuntimeType
                | _, true -> t.RuntimeType)
            |> Seq.toArray
        let elementTypes =
            elements |> Array.map snd
        
        //TODO: Make provided type here!
        let erasedType, erasedTypeFactory, getNth =
            match elementTypes with
            | [||] -> 
                typeof<unit>, 
                (fun _ -> Expr.Value(())), 
                (fun _ _ -> Expr.Value(()))
            | [|e|] -> 
                e, List.head, fun expr _ -> expr
            | _ -> 
                FSharpType.MakeTupleType elementTypes, 
                Expr.NewTuple, 
                fun expr i -> Expr.TupleGet(expr, i)
        let t = ProvidedTypeDefinition(route.Action + "Parameters", Some erasedType, HideObjectMethods = true)
        elements |> Array.mapi (fun i (name, runtimeType) ->
            ProvidedProperty(name, runtimeType, [], GetterCode = fun args -> getNth args.[0] i))
        |> Array.iter t.AddMember
        erasedType, t, erasedTypeFactory

    let buildParseBody urlParts queryParams erasedType parameterFactory segs qs =        
        let partExprs =
            urlParts |> Seq.mapi (fun i (part, _) ->
                let element = <@ (%%segs : Path).[i] @>
                match part with
                | FixedPart name -> id, <@ %element = name @>.Raw, None
                | VariablePart(name, t) ->
                    let check, access = buildCheckAccess t element
                    let fullCheck = <@ (%%segs : Path).Length > i && %check @>      
                    id, fullCheck.Raw, access)
        let queryExprs =
            queryParams |> Seq.map (fun (KeyValue(_,(p, _))) ->
                let key = p.Key
                let element = <@ (%%qs : QueryParams).TryFind key @>
                let tryConvertExpr = buildTryConvert p.Type element
                let maybeVar = Var(p.Key, tryConvertExpr.Type)
                let prior = fun expr -> Expr.Let(maybeVar, tryConvertExpr, expr)
                let maybeExpr = Expr.Var maybeVar
                let check =
                    if p.IsRequired then
                        match p.Type with
                        | StringType ->
                            <@@ (%%maybeExpr : string option).IsSome @@>
                        | _ ->
                            let propInfo = maybeVar.Type.GetProperty("HasValue")
                            Expr.PropertyGet(maybeExpr, propInfo)
                    else Expr.Value true
                let access =
                    if p.IsRequired then
                        match p.Type with
                        | StringType ->
                            <@@ (%%maybeExpr : string option).Value @@>
                        | _ ->
                            let propInfo = maybeVar.Type.GetProperty("Value") 
                            Expr.PropertyGet(Expr.Var maybeVar, propInfo)
                    else maybeExpr
                prior, check, Some access)
        let priors, checkExprs, accessExprs = Seq.append partExprs queryExprs |> Seq.toList |> List.unzip3
        let completeCheck =
            let expectedLength = urlParts |> Seq.length
            checkExprs |> List.fold (fun acc check ->
                <@ %acc && %%check @>) <@ (%%segs : Path).Length = expectedLength @>
        let tupleExpr =
            accessExprs |> List.choose id |> parameterFactory
        let genericOption = typedefof<_ option>
        let specificOption = genericOption.MakeGenericType [| erasedType |]
        let cases = FSharpType.GetUnionCases(specificOption)
        let none, some = cases.[0], cases.[1]
        let testAndCreateExpr =
            Expr.IfThenElse(completeCheck, 
                Expr.NewUnionCase(some, [tupleExpr]), 
                Expr.NewUnionCase(none, []))
        let fullExpr =
            priors |> List.fold (fun acc f ->
                f acc) testAndCreateExpr
        fullExpr

    let buildParseRequestExpr erasedType parameterFactory urlParts queryParams =
        let segsVar = Var("segs", typeof<Path>)
        let qsVar = Var("qs", typeof<QueryParams>)
        Expr.Lambda(segsVar,
            Expr.Lambda(qsVar,
                buildParseBody 
                    urlParts queryParams 
                    erasedType parameterFactory 
                    (Expr.Var segsVar) (Expr.Var qsVar)))

    let private generateUrlParamInfo (param : ParameterMetadata) =
        let key = param.Key
        ProvidedParameter(param.Key, param.Type.RuntimeType),
        fun req arg -> 
            let arg = Expr.Coerce(arg, typeof<obj>)
            <@ (%req : RouteUriBuilder).AddUrlParameter(key, (%%arg).ToString()) @>
    
    let private generateQueryParamInfo (param : ParameterMetadata) =
        let key = param.Key
        if param.IsRequired then
            ProvidedParameter(param.Key, param.Type.RuntimeType),
            fun req arg ->
                let arg = Expr.Coerce(arg, typeof<obj>)
                <@ (%req : RouteUriBuilder).AddQueryParameter(key, (%%arg).ToString()) @>
        else
            let optionalType = typedefof<_ option>.MakeGenericType [| param.Type.RuntimeType |]
            let valueProp = optionalType.GetProperty("Value")
            ProvidedParameter(param.Key, optionalType, optionalValue = null),
            fun req arg ->
                let argAsObj = Expr.Coerce(arg, typeof<obj>)
                let value = Expr.Coerce(Expr.PropertyGet(arg, valueProp), typeof<obj>)
                <@ 
                    if not(obj.ReferenceEquals(%%argAsObj, null)) then
                        (%req : RouteUriBuilder).AddQueryParameter(key, (%%value).ToString()) @>


    let buildConstructRouteMethodParams urlParts queryParams =
        let parameters = 
            urlParts |> Array.choose (fun (part, _) ->
                match part with
                | FixedPart _ -> None
                | VariablePart(name, t) -> Some { Key = name; Type = t; IsRequired = true })
        let queryParameters = queryParams |> Map.toSeq |> Seq.map (snd >> fst)
        [
            for p in parameters do yield generateUrlParamInfo p
            for p in queryParameters do yield generateQueryParamInfo p
        ]

    let buildUrlPartsExpr urlParts =
        let elementExprs = urlParts |> Array.map snd |> Array.toList
        Expr.NewArray(typeof<UrlPart>, elementExprs)

    let buildConstructRouteMethod urlParts queryParams =
        let partsExpr = buildUrlPartsExpr urlParts
        let allParams = buildConstructRouteMethodParams urlParts queryParams
        let builderVar = Var("builder", typeof<RouteUriBuilder>)
        let builderExpr = Expr.Var builderVar
        let createExpr (args : Expr list) =
            let argExpr i = args.[i+1]
            let setExprs =
                allParams |> List.map snd |> List.mapi (fun i setter ->
                    setter <@ %%builderExpr : RouteUriBuilder @> (argExpr i))
            let bodyExpr =
                if setExprs |> List.isEmpty then builderExpr
                else
                    let setAllExpr = setExprs |> List.map (fun e -> e.Raw) |> List.reduce(fun x y -> Expr.Sequential(x, y))
                    Expr.Sequential(setAllExpr, builderExpr)
            Expr.Let(
                builderVar, <@@ RouteUriBuilder(%%partsExpr) @@>, 
                Expr.Sequential(bodyExpr, <@ (%%builderExpr : RouteUriBuilder).Build() @>))
        ProvidedMethod(
            "CreateUri",
            allParams |> List.map fst,
            typeof<string>,
            InvokeCode = createExpr)
            
    let constructorFactory (route : Route) parameterType erasedType metadataExpr parseRequestExpr =
        let genericFactory =
            match route.Method with
            | Get -> typedefof<GetRequestHandlerFactory<_>>
            | Post -> typedefof<PostRequestHandlerFactory<_>>
            | Put -> typedefof<PutRequestHandlerFactory<_>>
            | Delete -> typedefof<DeleteRequestHandlerFactory<_>>
        let propType = genericFactory.MakeGenericType [| parameterType :> Type |]
        let consExpr =
            let erasedPropType = genericFactory.MakeGenericType [| erasedType |]
            let cons = erasedPropType.GetConstructors().[0]
            Expr.NewObject(cons, [metadataExpr; parseRequestExpr])
        propType, consExpr

    let clientConstructorFactory (route : Route) parameterType erasedType metadataExpr parseRequestExpr =
        let genericFactory = typedefof<_ ClientRoute>
        let propType = genericFactory.MakeGenericType [| parameterType :> Type |]
        let consExpr =
            let erasedPropType = genericFactory.MakeGenericType [| erasedType |]
            let cons = erasedPropType.GetConstructors().[0]
            Expr.NewObject(cons, [parseRequestExpr])
        propType, consExpr

    let generateRouteProperty constructorFactory (route : Route) =
        let urlParts, queryParams, metadataExpr = generateMetadata route
        let erasedType, parameterType, parameterFactory = generateTypeAndFactory route urlParts queryParams
        let parseRequestExpr = buildParseRequestExpr erasedType parameterFactory urlParts queryParams
        let propType, consExpr = constructorFactory route parameterType erasedType metadataExpr parseRequestExpr
        let extendedType =
            ProvidedTypeDefinition(route.Action + "Factory", Some propType, HideObjectMethods = true)
        extendedType.AddMember(buildConstructRouteMethod urlParts queryParams)
        extendedType.AddMember parameterType
        let property = 
            ProvidedProperty(
                route.Action, extendedType,
                IsStatic = true,
                GetterCode = fun _ -> consExpr)
        property.AddXmlDoc(route.UrlFormat + " : " + route.Description)
        seq {
            yield extendedType :> MemberInfo
            yield upcast property
        }

    let generateRoutes (routesFormat : string) parseRoute routeFactoryType =
        let routes = 
            routesFormat.Split([|'\r';'\n'|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map parseRoute
        routes |> Seq.groupBy (fun route -> route.Resource)
        |> Seq.map (fun (resource, subRoutes) ->
            let t = ProvidedTypeDefinition(resource, Some typeof<obj>, HideObjectMethods = true)
            subRoutes |> Seq.collect (generateRouteProperty routeFactoryType) |> Seq.iter t.AddMember
            t)
        |> Seq.toArray


[<TypeProvider>]
type RouteImpl(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()
    
    let ass = Assembly.GetExecutingAssembly()
    let nspace = "TypeInferred.HashBang"

    let generateTypeGraph requestedName routesFormat =
        let root = ProvidedTypeDefinition(ass, nspace, requestedName, Some typeof<obj>, HideObjectMethods = true)
        Generation.generateRoutes routesFormat Generation.parseRoute Generation.constructorFactory
        |> Array.iter root.AddMember
        root
    do
        this.RegisterRuntimeAssemblyLocationAsProbingFolder config
        config.ReferencedAssemblies 
        |> Array.map Path.GetDirectoryName 
        |> Seq.distinct |> Seq.iter this.RegisterProbingFolder
        let providerType = ProvidedTypeDefinition(ass, nspace, "RoutesProvider", None)
        let schemaUrl = ProvidedStaticParameter("RoutesFormat", typeof<string>)
        providerType.DefineStaticParameters(
            [schemaUrl], 
            fun requestedName -> 
            function
                | [| :? string as url |] -> generateTypeGraph requestedName url
                | _ -> failwith "Invalid parameters")
        this.AddNamespace(nspace, [providerType])

[<TypeProvider>]
type ClientRouteImpl(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()
    
    let ass = Assembly.GetExecutingAssembly()
    let nspace = "TypeInferred.HashBang"

    let generateTypeGraph requestedName routesFormat =
        let root = ProvidedTypeDefinition(ass, nspace, requestedName, Some typeof<obj>, HideObjectMethods = true)
        Generation.generateRoutes routesFormat Generation.parseClientRoute Generation.clientConstructorFactory
        |> Array.iter root.AddMember
        root
    do
        this.RegisterRuntimeAssemblyLocationAsProbingFolder config
        config.ReferencedAssemblies 
        |> Array.map Path.GetDirectoryName 
        |> Seq.distinct |> Seq.iter this.RegisterProbingFolder
        let providerType = ProvidedTypeDefinition(ass, nspace, "ClientRoutesProvider", None)
        let schemaUrl = ProvidedStaticParameter("RoutesFormat", typeof<string>)
        providerType.DefineStaticParameters(
            [schemaUrl], 
            fun requestedName -> 
            function
                | [| :? string as url |] -> generateTypeGraph requestedName url
                | _ -> failwith "Invalid parameters")
        this.AddNamespace(nspace, [providerType])


[<assembly : TypeProviderAssembly>]
do ()