﻿module TypeInferred.HashBang.Html.Implementation

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

    let numericChars = "0123456789".ToCharArray() |> set
    let generateName (cls:string) =
        let removeFrontNumeric (cls:string) =
            if numericChars.Contains cls.[0] then "_" + cls
            else cls
        cls.Replace('-', '_')
        |> removeFrontNumeric

    let generateTypeGraph requestedName schemaUrl shouldMonitor =
        let root = ProvidedTypeDefinition(ass, nspace, requestedName, None)
        let css = File.ReadAllTextRelativeAbsoluteOrHttp(config.ResolutionFolder, schemaUrl)
        if shouldMonitor then
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
        let shouldMonitorChanges = ProvidedStaticParameter("ShouldMonitorChanges", typeof<bool>)
        providerType.DefineStaticParameters(
            [schemaUrl; shouldMonitorChanges], 
            fun requestedName -> 
            function
                | [| :? string as url; :? bool as shouldMonitor |] -> 
                    generateTypeGraph requestedName url shouldMonitor
                | _ -> failwith "Invalid parameters")
        this.AddNamespace(nspace, [providerType])


/// Pulls in resource files for FunScript. 
/// We may later add better support inside FunScript for this scenario.
[<TypeProvider>]
type ResourceProviderImpl(config : TypeProviderConfig) as this =
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
        try Some(File.ReadAllTextRelativeAbsoluteOrHttp(config.ResolutionFolder, url))
        with _ -> None

    let generateTypeGraph requestedName schemaUrl =
        let root = ProvidedTypeDefinition(ass, nspace, requestedName, None)
        let ext = Path.GetExtension schemaUrl
        let imageMime = 
            Runtime.ContentTypes.fromExtension ext
            |> Option.bind (fun t ->
                if t.Mime.StartsWith "image" then Some t.Mime
                else None)
        match imageMime with
        | None ->
            let contents = File.ReadAllTextRelativeAbsoluteOrHttp(config.ResolutionFolder, schemaUrl)
            ProvidedProperty("RawTextContents", typeof<string>, IsStatic = true, GetterCode = fun _ -> <@@ contents @@>)
            |> root.AddMember
        | Some mime ->
            let contents = File.ReadAllBytesRelativeAbsoluteOrHttp(config.ResolutionFolder, schemaUrl)
            let base64 = Convert.ToBase64String contents
            let data = sprintf "data:%s;base64,%s" mime base64
            ProvidedProperty("RawImageData", typeof<string>, IsStatic = true, GetterCode = fun _ -> <@@ data @@>)
            |> root.AddMember
        root
    do
        this.RegisterRuntimeAssemblyLocationAsProbingFolder config
        config.ReferencedAssemblies 
        |> Array.map Path.GetDirectoryName 
        |> Seq.distinct |> Seq.iter this.RegisterProbingFolder
        let providerType = ProvidedTypeDefinition(ass, nspace, "ResourceProvider", None)
        let schemaUrl = ProvidedStaticParameter("FileOrUrl", typeof<string>)
        providerType.DefineStaticParameters(
            [schemaUrl], 
            fun requestedName -> 
            function
                | [| :? string as url |] -> 
                    generateTypeGraph requestedName url
                | _ -> failwith "Invalid parameters")
        this.AddNamespace(nspace, [providerType])

[<assembly : TypeProviderAssembly>]
do ()