module internal TypeInferred.HashBang.Owin.ClientSide

open System
open FunScript.TypeScript
open TypeInferred.HashBang.Owin.Utilities
open TypeInferred.HashBang.Routing
open TypeInferred.HashBang.Html

let createPageScript(handlers, errorPageTemplate) =
    <@
        let handlers = %handlers
        let resources = ref None

        let update() =
            let address = Globals.window.location.href
            let i = address.IndexOf("#!") 
            let path = 
                if i = -1 then "/"
                else address.Substring(i + 2, address.Length - i - 2)
            let segments, queryParams = WebUtility.SplitRelativeUri path
            async {
                let! newHtml = handlers |> List.tryPickAsync (fun f -> f segments queryParams)
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