module TypeInferred.HashBang.Routing.TypeGeneration

open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open TypeInferred.HashBang.Routing.ProvidedTypes
open System
open Domain
open Conversion

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
    urlParts, queryParams

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

let clientConstructorFactory (route : Route) parameterType erasedType parseRequestExpr =
    let genericFactory = typedefof<_ ClientRoute>
    let propType = genericFactory.MakeGenericType [| parameterType :> Type |]
    let consExpr =
        let erasedPropType = genericFactory.MakeGenericType [| erasedType |]
        let cons = erasedPropType.GetConstructors().[0]
        Expr.NewObject(cons, [parseRequestExpr])
    propType, consExpr

let generateRouteProperty (route : Route) =
    let urlParts, queryParams = generateMetadata route
    let erasedType, parameterType, parameterFactory = generateTypeAndFactory route urlParts queryParams
    let parseRequestExpr = buildParseRequestExpr erasedType parameterFactory urlParts queryParams
    let propType, consExpr = clientConstructorFactory route parameterType erasedType parseRequestExpr
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

let generateRoutes (routesFormat : string) =
    let routes = 
        routesFormat.Split([|'\r';'\n'|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map parseClientRoute
    routes |> Seq.groupBy (fun route -> route.Resource)
    |> Seq.map (fun (resource, subRoutes) ->
        let t = ProvidedTypeDefinition(resource, Some typeof<obj>, HideObjectMethods = true)
        subRoutes |> Seq.collect generateRouteProperty |> Seq.iter t.AddMember
        t)
    |> Seq.toArray