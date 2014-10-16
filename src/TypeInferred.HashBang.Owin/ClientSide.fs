module internal TypeInferred.HashBang.Owin.ClientSide

open System
open Microsoft.FSharp.Quotations
open FunScript.TypeScript
open TypeInferred.HashBang
open TypeInferred.HashBang.Owin.Utilities
open TypeInferred.HashBang.Routing
open TypeInferred.HashBang.Html

let createPages(pageTypes : System.Type list) =
    let pageExprs =
        pageTypes |> List.map (fun pageType ->
            let consInfo = pageType.GetConstructors().[0]
            let parameters =
                consInfo.GetParameters() |> Array.map (fun parameter ->
                    Expr.Value(null, parameter.ParameterType))
                |> Array.toList
            Expr.Coerce(Expr.NewObject(consInfo, parameters), typeof<IPage>))
    Expr.NewArray(typeof<IPage>, pageExprs)

let createPageScript(pageTypes, errorPageTemplate) =
    let pages = createPages pageTypes
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
            let request = { Path = segments; QueryParams = queryParams }
            async {
                let! newHtml = pages |> List.tryPickAsync (fun (p : IPage) -> p.TryHandle request)
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