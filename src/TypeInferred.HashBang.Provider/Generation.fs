module TypeInferred.HashBang.Provider.Generation

open System
open System.Reflection
open Microsoft.FSharp.Quotations
open Samples.FSharp.ProvidedTypes
open TypeInferred.HashBang
open TypeInferred.HashBang.Runtime
open TypeInferred.HashBang.Provider.Runtime
open ProviderImplementation.QuotationBuilder

type private ParamType =
    | HeaderParameter
    | UrlParameter

let private typeOfExpr t = 
    let fsCore = typedefof<list<_>>.Assembly
    let operators = fsCore.GetType("Microsoft.FSharp.Core.Operators")
    Expr.Call(operators.GetMethod("TypeOf").MakeGenericMethod [| t |], [ ])

let private castExpr t =
    let obj = Var("untypedObj", typeof<obj>)
    Expr.Lambda(obj, Expr.Coerce(Expr.Var obj, t))

let private asyncType t =
    typedefof<Async<_>>.MakeGenericType [| t |]

let private generateParameterInfo paramType (param : ParameterMetadata) =
    let setter req arg = 
        let key = param.Key
        match paramType with
        | UrlParameter -> <@@ (%%req : ApiRequest).AddParameter(key, (%%arg).ToString()) @@>
        | HeaderParameter -> <@@ (%%req : ApiRequest).AddParameter(key, (%%arg).ToString()) @@>
    param.Key, param.Type.RuntimeType, setter
    
let private generateBodyInfo (meta : TypeMetadata) =
    let t = meta.RuntimeType
    let tExpr = typeOfExpr t
    let setter req arg = 
        <@@ (%%req : ApiRequest).AddBody(Serialization.serialize (%%tExpr : Type) (%%arg : obj)) @@>
    "body", t, setter

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
    | Some(Application Json, meta) ->
        let t = meta.RuntimeType
        let asyncT = asyncType t
        let tExpr = typeOfExpr t
        asyncT, 
        fun req context -> 
            <@@ (%%req : ApiRequest).SendAndDeserialize(%%tExpr, (%%context : ApiDataContext)) @@> |> typeAsync t
    | Some _ -> failwith "Not implemented: only application/json is supported." 

let private generateParams (handler : HandlerMetadata) =
    let requiredParams, optionalParams = 
        handler.Parameters |> Array.partition (fun p -> p.IsRequired)
    let requiredHeaders, optionalHeaders = 
        handler.Headers |> Array.partition (fun p -> p.IsRequired)
    let requiredParams =
        [
            for p in requiredHeaders do yield generateParameterInfo HeaderParameter p
            for p in requiredParams do yield generateParameterInfo UrlParameter p
            match handler.RequestType with
            | None -> ()
            | Some (Application Json, meta) -> yield generateBodyInfo meta
            | Some _ -> failwith "Not implemented: only application/json is supported."
        ] |> List.map (fun (name, t, setter) -> ProvidedParameter(name, t), setter)
    requiredParams

let private generateActionMethod (handler : HandlerMetadata) =
    let requiredParams = generateParams handler
//    let optParams =
//        [
//            for p in optionalHeaders do yield generateParameterInfo HeaderParameter p
//            for p in optionalParams do yield generateParameterInfo UrlParameter p
//        ] |> List.map (fun (name, t, setter) -> ProvidedParameter(name, t), setter)
    let returnType, returnExpr = genReturnTypeAndExpr handler
    let requestVar = Var("request", typeof<ApiRequest>)
    let requestExpr = Expr.Var requestVar
    let parts = marshallParts handler.UrlParts //TODO: Perf?
    let methodName = handler.Method.HttpName
    let sendExpr (args : Expr list) =
        let argExpr i = Expr.Coerce(args.[i+1], typeof<obj>)
        let setExprs =
            requiredParams |> List.map snd |> List.mapi (fun i setter ->
                setter requestExpr (argExpr i))
        let bodyExpr =
            if setExprs |> List.isEmpty then requestExpr
            else
                let setAllExpr = setExprs |> List.reduce(fun x y -> Expr.Sequential(x, y))
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