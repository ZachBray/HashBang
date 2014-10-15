namespace TypeInferred.HashBang.SignalR

/// A collection of services that can be called by name with serialized parameters.
type internal ServiceCollection(boxedServices) =

    let serviceLookup = 
        boxedServices |> Array.map (fun boxedService ->
            let router = MemberFuncCache(boxedService)
            router.ServiceName, router)
        |> dict

    member __.CallAsyncMember(serviceName, memberName, memberTypeArguments, memberArguments) =
        match serviceLookup.TryGetValue serviceName with
        | false, _ -> failwithf "Cannot find service called '%s'." serviceName
        | true, service -> service.CallAsyncMember(memberName, memberTypeArguments, memberArguments)

    member __.CallObservableMember(serviceName, memberName, memberTypeArguments, memberArguments) =
        match serviceLookup.TryGetValue serviceName with
        | false, _ -> failwithf "Cannot find service called '%s'." serviceName
        | true, service -> service.CallObservableMember(memberName, memberTypeArguments, memberArguments)
