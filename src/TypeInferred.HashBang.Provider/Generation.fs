module TypeInferred.HashBang.Provider.Generation

open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Samples.FSharp.ProvidedTypes
open TypeInferred.HashBang
open TypeInferred.HashBang.Runtime
open TypeInferred.HashBang.Provider.Runtime
open ProviderImplementation.QuotationBuilder

let private typeOfExpr t = 
    let fsCore = typedefof<list<_>>.Assembly
    let operators = fsCore.GetType("Microsoft.FSharp.Core.Operators")
    Expr.Call(operators.GetMethod("TypeOf").MakeGenericMethod [| t |], [ ])

let private castExpr t =
    let obj = Var("untypedObj", typeof<obj>)
    Expr.Lambda(obj, Expr.Coerce(Expr.Var obj, t))

let private asyncType t =
    typedefof<Async<_>>.MakeGenericType [| t |]

let private generateHeaderParamInfo (param : ParameterMetadata) =
    let key = param.Key
    ProvidedParameter(param.Key, param.Type.RuntimeType),
    fun req arg ->
        let arg = Expr.Coerce(arg, typeof<obj>) 
        <@ (%req : ApiRequest).AddHeader(key, (%%arg).ToString()) @>

let private generateUrlParamInfo (param : ParameterMetadata) =
    let key = param.Key
    ProvidedParameter(param.Key, param.Type.RuntimeType),
    fun req arg -> 
        let arg = Expr.Coerce(arg, typeof<obj>)
        <@ (%req : ApiRequest).AddUrlParameter(key, (%%arg).ToString()) @>
    
let private generateQueryParamInfo (param : ParameterMetadata) =
    let key = param.Key
    if param.IsRequired then
        ProvidedParameter(param.Key, param.Type.RuntimeType),
        fun req arg ->
            let arg = Expr.Coerce(arg, typeof<obj>)
            <@ (%req : ApiRequest).AddQueryParameter(key, (%%arg).ToString()) @>
    else
        let optionalType = typedefof<_ option>.MakeGenericType [| param.Type.RuntimeType |]
        let valueProp = optionalType.GetProperty("Value")
        ProvidedParameter(param.Key, optionalType, optionalValue = null),
        fun req arg ->
            let argAsObj = Expr.Coerce(arg, typeof<obj>)
            let value = Expr.Coerce(Expr.PropertyGet(arg, valueProp), typeof<obj>)
            <@ 
                if not(obj.ReferenceEquals(%%argAsObj, null)) then
                    (%req : ApiRequest).AddQueryParameter(key, (%%value).ToString()) @>
            
    
let private generateBodyInfo (meta : TypeMetadata) =
    let t = meta.RuntimeType
    let tExpr = typeOfExpr t
    let setter req arg = 
        let arg = Expr.Coerce(arg, typeof<obj>)
        <@ (%req : ApiRequest).AddBody(Serialization.serialize (%%tExpr : Type) (%%arg : obj)) @>
    ProvidedParameter("body", t), setter

let private serializeParts = precomputeToJson<UrlPart[]>()
let private marshallParts parts =
    let json = serializeParts parts
    Expr.Coerce(<@@ Serialization.deserialize typeof<UrlPart[]> json @@>, typeof<UrlPart[]>)

let private genReturnTypeAndExpr (handler : HandlerMetadata) =
    match handler.ResponseType with
    | None -> 
        typeof<unit Async>, 
        fun req context -> 
            <@@ (%%req : ApiRequest).SendIgnore(%%context : ApiDataContext) @@>
    | Some (contentType, meta) when contentType = ContentTypes.Application.json ->
        let t = meta.RuntimeType
        let asyncT = asyncType t
        let tExpr = typeOfExpr t
        asyncT, 
        fun req context -> 
            <@@ (%%req : ApiRequest).SendAndDeserialize(%%tExpr, (%%context : ApiDataContext)) @@> |> typeAsync t
    | Some _ -> failwith "Not implemented: only application/json is supported." 

let private generateParams (handler : HandlerMetadata) =
    [
        for p in handler.Headers do yield generateHeaderParamInfo p
        for p in handler.Parameters do yield generateUrlParamInfo p
        for p in handler.QueryParameters do yield generateQueryParamInfo p
        match handler.RequestType with
        | None -> ()
        | Some (contentType, meta) when contentType = ContentTypes.Application.json -> yield generateBodyInfo meta
        | Some _ -> failwith "Not implemented: only application/json is supported."
    ]

let private generateActionMethod (handler : HandlerMetadata) =
    let requiredParams = generateParams handler
    let returnType, returnExpr = genReturnTypeAndExpr handler
    let requestVar = Var("request", typeof<ApiRequest>)
    let requestExpr = Expr.Var requestVar
    let parts = marshallParts handler.UrlParts //TODO: Perf?
    let methodName = handler.Method.HttpName
    let sendExpr (args : Expr list) =
        let argExpr i = args.[i+1]
        let setExprs =
            requiredParams |> List.map snd |> List.mapi (fun i setter ->
                setter <@ %%requestExpr : ApiRequest @> (argExpr i))
        let bodyExpr =
            if setExprs |> List.isEmpty then requestExpr
            else
                let setAllExpr = setExprs |> List.map (fun e -> e.Raw) |> List.reduce(fun x y -> Expr.Sequential(x, y))
                Expr.Sequential(setAllExpr, requestExpr)
        Expr.Let(
            requestVar, <@@ ApiRequest(methodName, %%parts) @@>, 
            Expr.Sequential(bodyExpr, returnExpr (Expr.Var requestVar) args.[0]))
    ProvidedMethod(
        handler.Action,
        requiredParams |> List.map fst,
        returnType,
        InvokeCode = sendExpr)



let private generateResourceProperty (domain : ProvidedTypeDefinition) (resourceName, handlers : HandlerMetadata seq) =
    let t = ProvidedTypeDefinition(resourceName + "Resource", Some typeof<ApiDataContext>)
    t.AddMembers [
        for handler in handlers do
            let meth = generateActionMethod handler
            meth.AddXmlDoc handler.Description
            yield meth
    ]
    domain.AddMember t
    ProvidedProperty(
        resourceName, t, 
        GetterCode = fun args -> <@@ (%%args.[0] : ApiDataContext) @@>)

let generateFrom (handlers : HandlerMetadata[]) =
    let domain = ProvidedTypeDefinition("DomainTypes", None)
    let context = ProvidedTypeDefinition("DataContext", Some typeof<ApiDataContext>)
    handlers |> Seq.groupBy (fun h -> h.Resource)
    |> Seq.map (generateResourceProperty domain)
    |> Seq.toList
    |> context.AddMembers
    context, domain