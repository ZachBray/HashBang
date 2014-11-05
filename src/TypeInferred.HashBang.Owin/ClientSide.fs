module internal TypeInferred.HashBang.Owin.ClientSide

open System
open Microsoft.FSharp.Quotations
open FunScript.TypeScript
open TypeInferred.HashBang
open TypeInferred.HashBang.Owin.Utilities
open TypeInferred.HashBang.Routing
open TypeInferred.HashBang.Html
open System.Collections.Generic

[<FunScript.JS>]
module internal Singleton =
    let instances = Dictionary<string, obj>()


let rec construct(isSingletonType, t : System.Type) =
    if ServiceEx.isAHashBangService t then Expr.Value(null, t)
    else
        let constructors = t.GetConstructors()
        if constructors.Length <> 1 then failwith "Multiple constuctors not supported in IPage implementation constructor parameters."
        let consInfo = constructors.[0]
        let args =
            consInfo.GetParameters() |> Array.map (fun parameter ->
                construct(isSingletonType, parameter.ParameterType))
            |> Array.toList
        let constructionExpr = Expr.NewObject(consInfo, args)
        if isSingletonType t then
            let boxedConstructionExpr = Expr.Coerce(constructionExpr, typeof<obj>)
            let typeName = t.FullName
            let findOrCreateExpr =
                <@@
                    if not (Singleton.instances.ContainsKey typeName)then
                        Singleton.instances.Add(typeName, %%boxedConstructionExpr)
                    Singleton.instances.[typeName]
                @@>
            Expr.Coerce(findOrCreateExpr, t)
        else constructionExpr

let createPages(isSingletonType, pageTypes : System.Type list) =
    let pageExprs =
        pageTypes |> List.map (fun pageType ->
            let constructionExpr = construct(isSingletonType, pageType)
            Expr.Coerce(constructionExpr, typeof<IPage>))
    Expr.NewArray(typeof<IPage>, pageExprs)

let createPageScript(isSingletonType, pageTypes, errorPageTemplate) =
    let pages = createPages(isSingletonType, pageTypes)
    <@
        let pages  = (%%pages : IPage[]) |> Array.toList
        let resources = ref None

        let update() =
            let address = Globals.window.location.href
            let i = address.IndexOf("#!") 
            let path = 
                if i = -1 then "/"
                else address.Substring(i + 2, address.Length - i - 2)
            let segments, queryParams = WebUtility.SplitRelativeUri path
            async {
                let! newHtml = pages |> List.tryPickAsync (fun (p : IPage) -> p.RequestHandler segments queryParams)
                let newHtml =
                    match newHtml with
                    | None -> (%errorPageTemplate) ("Error! Unable to find: " + path)
                    | Some html -> html
                !resources |> Option.iter (fun (r:IDisposable) -> r.Dispose())
                resources := Some(Globals.Dollar.Invoke("body").html(newHtml))
            } |> Async.StartImmediate
        
        Globals.onhashchange <- fun _ -> update(); null
        update()
    @>