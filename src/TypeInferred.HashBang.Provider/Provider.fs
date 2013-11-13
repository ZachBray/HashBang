namespace TypeInferred.HashBang.Provider

open System.Reflection
open System.IO
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
open TypeInferred.HashBang
open Runtime

[<TypeProvider>]
type HashBangAPIProviderImpl(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()
    
    let ass = Assembly.GetExecutingAssembly()
    let nspace = "TypeInferred.HashBang"

    let deserialize = precomputeFromJson<HandlerMetadata[]>()

    let tryReadAllText url =
        try Some(File.ReadAllTextRelativeAbsoluteOrHttp(config.ResolutionFolder, url))
        with _ -> None

    let rec checkForChanges(json, schemaUrl) = async {
        do! Async.Sleep 10000
        match tryReadAllText schemaUrl with
        | None -> this.Invalidate()
        | Some foundJson ->
            if json = foundJson then return! checkForChanges(json, schemaUrl)
            else this.Invalidate()
    }

    let generateTypeGraph requestedName schemaUrl =
        let root = ProvidedTypeDefinition(ass, nspace, requestedName, None)
        let json = File.ReadAllTextRelativeAbsoluteOrHttp(config.ResolutionFolder, schemaUrl)
        checkForChanges(json, schemaUrl) |> Async.Start
        let handlers = deserialize json
        let dataContext, domainTypes = Generation.generateFrom handlers
        root.AddMember dataContext
        root.AddMember domainTypes
        let baseUrl = ProvidedParameter("baseUrl", typeof<string>)
        ProvidedMethod(
            "GetDataContext", [baseUrl], dataContext,
            IsStaticMethod = true,
            InvokeCode = fun args ->
                <@@
                    { BaseUrl = %%(args.[0]) }
                @@>)
        |> root.AddMember
        root

    do
        this.RegisterRuntimeAssemblyLocationAsProbingFolder config
        config.ReferencedAssemblies 
        |> Array.map Path.GetDirectoryName 
        |> Seq.distinct |> Seq.iter this.RegisterProbingFolder
        let providerType = ProvidedTypeDefinition(ass, nspace, "HashBangAPI", None)
        let schemaUrl = ProvidedStaticParameter("SchemaUrl", typeof<string>)
        providerType.DefineStaticParameters(
            [schemaUrl], 
            fun requestedName -> 
            function
                | [| :? string as url |] -> generateTypeGraph requestedName url
                | _ -> failwith "Invalid parameters")
        this.AddNamespace(nspace, [providerType])


[<assembly : TypeProviderAssembly>]
do ()