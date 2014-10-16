namespace TypeInferred.HashBang.Owin

open System
open System.Reflection
open System.Threading.Tasks
open System.Collections.Generic
open Microsoft.Owin
open Yahoo.Yui.Compressor
open FunScript
open TypeInferred.HashBang
open TypeInferred.HashBang.SignalR
open TypeInferred.HashBang.Routing
open TypeInferred.HashBang.Html
open TypeInferred.HashBang.MimeTypes
open TypeInferred.HashBang.Owin.Utilities

type Environment = IDictionary<string, obj>
type Continuation = Func<Environment, Task>

type HashBangMiddleware(next, options:HashBangOptions) =
    inherit OwinMiddleware(next)
    let funScriptComponentInjector = fun existingComponents ->
        options.FunScriptComponentInjector(existingComponents @ FunScriptInterop.components)

    let compressor = lazy JavaScriptCompressor()

    let compileScript script =
        let isCompressionEnabled = options.IsJavaScriptCompressionEnabled
        let raw = FunScript.Compiler.Compiler.Compile(script, funScriptComponentInjector, noReturn = true, shouldCompress = isCompressionEnabled)
        if isCompressionEnabled then compressor.Value.Compress raw
        else raw

    let pageTypes =
        options.Pages |> List.map (fun page ->
            let pageType = page.GetType()
            match pageType.GetConstructors() with
            | [|consInfo|] ->
                let areAllParametersServices =
                    consInfo.GetParameters() |> Array.forall (fun parameter ->
                        ServiceEx.isAHashBangService parameter.ParameterType)
                if not areAllParametersServices then 
                    failwithf "Pages can only take service types as constructor parameters but found: %s" pageType.FullName
                else pageType 
            | _ -> failwithf "Pages must have exactly one constructor."
        )

    let compilePage page =
        let script = ClientSide.createPageScript(pageTypes, options.ErrorPageTemplate)
        let compiledScript = compileScript script
        let hashBangScript = Script.empty |> Element.appendText [compiledScript]
        options.ServerServedPageTemplate(page, hashBangScript) 
        |> Compiler.compilePage

    let findPage request =
        options.Pages |> List.tryPickAsync (fun page ->
            page.TryHandle request)
        |> Async.map (Option.map compilePage)
    
    let rootPage = findPage { Path = [||]; QueryParams = Map.empty } |> Async.cache

    let getPage url =
        match WebUtility.SplitRelativeUri url with
        | [||], queryParams when queryParams |> Map.isEmpty -> rootPage
        | path, queryParams -> findPage { Path = path; QueryParams = queryParams }

    let defaultHeaders = [|
        "Cache-Control", "no-cache"
        "Cache-Control", "no-store"
        "Cache-Control", "must-revalidate"
        "Pragma", "no-cache"
        "Expires", "0"
    |]

    let serve(context:IOwinContext, url) = async {
        let! page = getPage url
        match page with
        | None -> 
            return! Async.awaitTask(next.Invoke context)
        | Some page ->
            for key, value in defaultHeaders do
                context.Response.Headers.Append(key, value)
            context.Response.ContentType <- Text.html.Mime
            context.Response.StatusCode <- 200
            return! context.Response.WriteAsync page |> Async.awaitTask
    }

    override __.Invoke(context:IOwinContext) : Task =
        async {
            try
                if context.Request.Path.HasValue && context.Request.Path.Value = "/"  then
                    match context.Request.Query.["_escaped_fragment_"] with
                    | null -> return! serve(context, "/")
                    | crawlerPath -> return! serve(context, crawlerPath)
                else return! Async.awaitTask(next.Invoke context)
            with _ ->
                return! Async.awaitTask(next.Invoke context)
        } |> Async.StartAsTask :> Task
