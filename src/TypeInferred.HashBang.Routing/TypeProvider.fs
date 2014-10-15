namespace TypeInferred.HashBang.Routing

open System
open System.Reflection
open System.IO
open Microsoft.FSharp.Core.CompilerServices
open TypeInferred.HashBang.Routing.ProvidedTypes

[<TypeProvider>]
type RouteProviderImpl(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()
    
    let ass = Assembly.GetExecutingAssembly()
    let nspace = "TypeInferred.HashBang.Routing"

    let generateTypeGraph requestedName routesFormat =
        let root = ProvidedTypeDefinition(ass, nspace, requestedName, Some typeof<obj>, HideObjectMethods = true)
        TypeGeneration.generateRoutes routesFormat
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