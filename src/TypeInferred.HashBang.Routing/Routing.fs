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

    let buildParseBody urlParts queryParams parameterType parameterFactory segs qs =        
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
            checkExprs |> List.fold (fun acc check ->
                <@ %acc && %%check @>) <@ true @>
        let tupleExpr =
            accessExprs |> List.choose id |> parameterFactory
        let genericOption = typedefof<_ option>
        let specificOption = genericOption.MakeGenericType [| parameterType |]
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

    let buildParseRequestExpr parameterType parameterFactory urlParts queryParams =
        let segsVar = Var("segs", typeof<Path>)
        let qsVar = Var("qs", typeof<QueryParams>)
        Expr.Lambda(segsVar,
            Expr.Lambda(qsVar,
                buildParseBody 
                    urlParts queryParams 
                    parameterType parameterFactory 
                    (Expr.Var segsVar) (Expr.Var qsVar)))

    let generateRouteProperty(route : Route) =
        let urlParts, queryParams, metadataExpr = generateMetadata route
        let erasedType, parameterType, parameterFactory = generateTypeAndFactory route urlParts queryParams
        let parseRequestExpr = buildParseRequestExpr erasedType parameterFactory urlParts queryParams
        let genericFactory = 
            match route.Method with
            | Get -> typedefof<GetRequestHandlerFactory<_>>
            | Post -> typedefof<PostRequestHandlerFactory<_>>
            | Put -> typedefof<PutRequestHandlerFactory<_>>
            | Delete -> typedefof<DeleteRequestHandlerFactory<_>>
        let propType = genericFactory.MakeGenericType [| parameterType :> Type |]
        let completeExpr =
            let erasedPropType = genericFactory.MakeGenericType [| erasedType |]
            let cons = erasedPropType.GetConstructors().[0]
            Expr.NewObject(cons, [metadataExpr; parseRequestExpr])
        let property = 
            ProvidedProperty(
                route.Action, propType,
                IsStatic = true,
                GetterCode = fun _ -> completeExpr)
        property.AddXmlDoc(route.UrlFormat + " : " + route.Description)
        seq {
            yield parameterType :> MemberInfo
            yield upcast property
        }

    let generateRoutes (routesFormat : string) =
        let routes = 
            routesFormat.Split([|'\r';'\n'|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map parseRoute
        routes |> Seq.groupBy (fun route -> route.Resource)
        |> Seq.map (fun (resource, subRoutes) ->
            let t = ProvidedTypeDefinition(resource, Some typeof<obj>, HideObjectMethods = true)
            subRoutes |> Seq.collect generateRouteProperty |> Seq.iter t.AddMember
            t)
        |> Seq.toArray


[<TypeProvider>]
type RouteImpl(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()
    
    let ass = Assembly.GetExecutingAssembly()
    let nspace = "TypeInferred.HashBang"

    let generateTypeGraph requestedName routesFormat =
        let root = ProvidedTypeDefinition(ass, nspace, requestedName, Some typeof<obj>, HideObjectMethods = true)
        Generation.generateRoutes routesFormat
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


[<assembly : TypeProviderAssembly>]
do ()