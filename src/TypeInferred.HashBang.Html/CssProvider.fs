module TypeInferred.HashBang.Html.Implementation

open System
open System.Reflection
open System.IO
open System.Net
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open Samples.FSharp.ProvidedTypes
open TypeInferred.HashBang



[<TypeProvider>]
type CssProviderImpl(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()
    
    let ass = Assembly.GetExecutingAssembly()
    let nspace = "TypeInferred.HashBang"

    let parseClasses(content : string) =
        content.Split(
            [|' ';'\r';'\n';'\t';',';'{';'}';':';'>';'<';'+';'[';']';';'|], 
            StringSplitOptions.RemoveEmptyEntries)
        |> Seq.filter (fun term -> term.StartsWith ".")
        |> Seq.collect (fun classes -> classes.Split([|'.'|], StringSplitOptions.RemoveEmptyEntries))
        |> set

    let tryReadAllText url =
        try Some(File.ReadAllTextRelativeAbsoluteOrHttpText(config.ResolutionFolder, url))
        with _ -> None

    let rec checkForChanges(json, schemaUrl) = async {
        do! Async.Sleep 10000
        match tryReadAllText schemaUrl with
        | None -> this.Invalidate()
        | Some foundJson ->
            if json = foundJson then return! checkForChanges(json, schemaUrl)
            else this.Invalidate()
    }

    let numericChars = "0123456789".ToCharArray() |> set
    let generateName (cls:string) =
        let removeFrontNumeric (cls:string) =
            if numericChars.Contains cls.[0] then "_" + cls
            else cls
        cls.Replace('-', '_')
        |> removeFrontNumeric

    let generateTypeGraph requestedName schemaUrl =
        let root = ProvidedTypeDefinition(ass, nspace, requestedName, None)
        let css = File.ReadAllTextRelativeAbsoluteOrHttpText(config.ResolutionFolder, schemaUrl)
        checkForChanges(css, schemaUrl) |> Async.Start

        ProvidedProperty("RawStyleSheet", typeof<string>, IsStatic = true, GetterCode = fun _ -> <@@ css @@>)
        |> root.AddMember

        let cssClasses = parseClasses css
        for cls in cssClasses do
            ProvidedProperty(generateName cls, typeof<string>, IsStatic = true, GetterCode = fun _ -> <@@ cls @@>)
            |> root.AddMember

        root
    do
        this.RegisterRuntimeAssemblyLocationAsProbingFolder config
        config.ReferencedAssemblies 
        |> Array.map Path.GetDirectoryName 
        |> Seq.distinct |> Seq.iter this.RegisterProbingFolder
        let providerType = ProvidedTypeDefinition(ass, nspace, "CssClassesProvider", None)
        let schemaUrl = ProvidedStaticParameter("CssFileOrUrl", typeof<string>)
        providerType.DefineStaticParameters(
            [schemaUrl], 
            fun requestedName -> 
            function
                | [| :? string as url |] -> generateTypeGraph requestedName url
                | _ -> failwith "Invalid parameters")
        this.AddNamespace(nspace, [providerType])


[<assembly : TypeProviderAssembly>]
do ()