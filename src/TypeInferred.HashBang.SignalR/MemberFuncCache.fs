namespace TypeInferred.HashBang.SignalR

open System
open System.Reflection
open System.Linq.Expressions
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Linq.RuntimeHelpers

/// Caches Funcs created from LINQ expressions that...
///     1. Deserialize arguments
///     2. Call service methods/properties
///     3. Serialize Async/Observable results
type internal MemberFuncCache(serviceType : Type, boxedService) =
    let serviceName = serviceType.FullName
    let serviceExpr =
        Expr.Coerce(Expr.Value(boxedService, boxedService.GetType()), serviceType) // cast to service interface

    let canClientAccess (propertyInfo : MemberInfo) =
        propertyInfo.GetCustomAttribute<ClientCannotAccessAttribute>() = Unchecked.defaultof<_>

    let memberLookup =
        serviceType.GetMembers(BindingFlags.Instance ||| BindingFlags.Public)
        |> Array.filter canClientAccess
        |> Array.filter (fun memberInfo -> memberInfo.MemberType = MemberTypes.Method 
                                        || memberInfo.MemberType = MemberTypes.Property)
        |> Array.map (fun memberInfo -> memberInfo.Name, memberInfo)
        |> dict

    let locateType typeFullName =
        let t =
            AppDomain.CurrentDomain.GetAssemblies()
            |> Array.tryPick (fun assembly -> 
                let t = assembly.GetType(typeFullName, throwOnError=false, ignoreCase=true)
                if t = Unchecked.defaultof<_> then None
                else Some t)
        match t with
        | None -> failwithf "Unable to locate type with FullName='%s'" typeFullName
        | Some t -> t

    let asyncName = typeof<_ Async>.Name
    let iobservableName = typeof<_ IObservable>.Name

    let createSerializeAsyncExpr(memberName, returnType : Type, argExpr) =
        let itemType = returnType.GetGenericArguments().[0]
        let serializerType = typedefof<CachedSerializer<_>>.MakeGenericType [|itemType|]
        let serializeMethod =
            match returnType.Name with
            | n when n = asyncName ->
                serializerType.GetMethod("SerializeAsync", BindingFlags.NonPublic ||| BindingFlags.Static)
            | n when n = iobservableName ->
                serializerType.GetMethod("SerializeObservable", BindingFlags.NonPublic ||| BindingFlags.Static)
            | _ -> failwithf "Expected a 'T Async/IObservable return type on '%s.%s.'" serviceName memberName
        Expr.Call(serializeMethod, [argExpr])

    let createDeserializeExpr(argType, argExpr) =
        let serializerType = typedefof<CachedSerializer<_>>.MakeGenericType [|argType|]
        let deserializeMethod = serializerType.GetMethod("Deserialize", BindingFlags.NonPublic ||| BindingFlags.Static)
        Expr.Call(deserializeMethod, [argExpr])

    let compilePropertyCall(memberName, propertyInfo : PropertyInfo) =
        let argsVar = Var("args", typeof<string[]>)
        let bodyExpr =
            createSerializeAsyncExpr(memberName, propertyInfo.PropertyType, Expr.PropertyGet(serviceExpr, propertyInfo))
        let returnType = propertyInfo.PropertyType.GetGenericTypeDefinition().MakeGenericType [|typeof<string>|]
        Expr.NewDelegate(Expression.GetFuncType [|argsVar.Type; returnType|], [argsVar], bodyExpr)
        |> LeafExpressionConverter.QuotationToExpression

    let compileMethodCall (memberName, methodInfo : MethodInfo) =
        let argsVar = Var("args", typeof<string[]>)
        let argsExpr = Expr.Var(argsVar)
        let argVar i = <@ (%%argsExpr : string[]).[i] @>
        let parameters = methodInfo.GetParameters()
        let deserializedArgs =
            parameters |> Array.mapi (fun i parameter -> createDeserializeExpr(parameter.ParameterType, argVar i))
            |> Array.toList
        let bodyExpr =
            createSerializeAsyncExpr(memberName, methodInfo.ReturnType, Expr.Call(serviceExpr, methodInfo, deserializedArgs))
        let returnType = methodInfo.ReturnType.GetGenericTypeDefinition().MakeGenericType [|typeof<string>|]
        Expr.NewDelegate(Expression.GetFuncType [|argsVar.Type; returnType|], [argsVar], bodyExpr)
        |> LeafExpressionConverter.QuotationToExpression

    let createExpression(memberName, typeArgNames, serializedArgCount) =
        match memberLookup.TryGetValue memberName with
        | true, (:? PropertyInfo as propertyInfo) -> 
            match typeArgNames, serializedArgCount with
            | [||], 0 -> compilePropertyCall(memberName, propertyInfo)
            | _ -> failwithf "Cannot call indexed/generic properties like '%s.%s'." serviceName memberName
        | true, (:? MethodInfo as methodInfo) ->
            let genericArgs = methodInfo.GetGenericArguments()
            if genericArgs.Length = typeArgNames.Length then
                let methodInfo =
                    match genericArgs.Length with
                    | 0 -> methodInfo
                    | _ ->
                        let typeArgs = typeArgNames |> Array.map locateType
                        methodInfo.MakeGenericMethod typeArgs
                compileMethodCall(memberName, methodInfo)
            else failwithf "Incorrect number of generic arguments were provided for '%s.%s.': %A" serviceName memberName typeArgNames
        |  _ -> failwithf "Cannot find public method or property called '%s.%s'." serviceName memberName         

    let createAsyncFunc(memberName, typeArgNames, serializedArgCount) =
        let expression =
            createExpression(memberName, typeArgNames, serializedArgCount)
            |> unbox<Expression<Func<string[], string Async>>>
        expression.Compile()

    let createObservableFunc(memberName, typeArgNames, serializedArgCount) =
        let expression =
            createExpression(memberName, typeArgNames, serializedArgCount)
            |> unbox<Expression<Func<string[], string IObservable>>>
        expression.Compile()

    let compiledAsyncFuncs = Cache(createAsyncFunc)
    let compiledObservableFuncs = Cache(createObservableFunc)

    member val ServiceName = serviceName

    member __.CallAsyncMember(memberName, memberTypeArguments, memberArguments) = 
        let func = compiledAsyncFuncs.FindOrCreate(memberName, memberTypeArguments, Array.length memberArguments)
        func.Invoke memberArguments

    member __.CallObservableMember(memberName, memberTypeArguments, memberArguments) = 
        let func =  compiledObservableFuncs.FindOrCreate(memberName, memberTypeArguments, Array.length memberArguments)
        func.Invoke memberArguments