namespace TypeInferred.HashBang

open System.Net
open Microsoft.FSharp.Linq
open Microsoft.FSharp.Quotations
open Yahoo.Yui.Compressor

open TypeInferred.HashBang.Html

type ElementId = string

type DynamicWebsiteHandler = Path -> QueryParams -> IHtmlTag option Async

type DynamicRoutes = RoutesProvider<"
GET /                                   # Site.User         # The application page for normal users
GET /?_escaped_fragment_=string         # Site.Crawler      # The application page for web crawlers
">

module Helpers =
    open System
    open FunScript

    [<JS>]
    module Replacements =

        let toNullable = function
            | Some x -> Nullable(x)
            | None -> Nullable()

        let tryParseInt32 x = Int32.TryParseJS x |> toNullable
        let tryParseUInt32 x = UInt32.TryParseJS x |> toNullable
        let tryParseDouble x = Double.TryParseJS x |> toNullable
        let tryParseBoolean x = Boolean.TryParseJS x |> toNullable
        let tryParseChar (x:string) = Nullable(x.[0])

    open Replacements

    let funScriptComponents =
        [
            ExpressionReplacer.create <@ Int32.TryConvert @> <@ tryParseInt32 @>
            ExpressionReplacer.create <@ UInt32.TryConvert @> <@ tryParseUInt32 @>
            ExpressionReplacer.create <@ Double.TryConvert @> <@ tryParseDouble @>
            ExpressionReplacer.create <@ Boolean.TryConvert @> <@ tryParseBoolean @>
            ExpressionReplacer.create <@ Char.TryConvert @> <@ tryParseChar @>
        ]

type DynamicWebsite =
    {
        Prefix : string
        HeadTemplate : HtmlTag<IHeadElement>
        BodyTemplate : HtmlTag<IBodyElement>
        CanCompressCode : bool
        ErrorBody : IHtmlTag Expr
        Sections : Map<ElementId, DynamicWebsiteHandler list> Expr
    }
    
    member private site.Main =
        let sectionHandlers = site.Sections
        let errorBody = site.ErrorBody
        <@@
            let sectionHandlers = %sectionHandlers |> Map.toArray
            let errorBody = %errorBody

            let update() =
                let address = Globals.window.location.href
                let i = address.IndexOf("#!") 
                let path = 
                    if i = -1 then "/"
                    else address.Substring(i + 2, address.Length - i - 2)
                let segments, queryParams = WebUtility.SplitRelativeUri path
                sectionHandlers |> Array.iter(fun (elementId, handlers) ->
                    async {
                        let! newHtml = handlers |> List.tryPickAsync (fun f -> f segments queryParams)
                        newHtml
                        |> Option.iter (fun newHtml ->
                            Globals.Dollar.Invoke("#" + elementId).html(newHtml) |> ignore)
                    } |> Async.StartImmediate)
                    

            Globals.onhashchange <- fun _ -> update(); null
            update()
        @@>

    static member At address =
        { 
            Prefix = address 
            HeadTemplate = Head.empty
            BodyTemplate = Body.empty
            CanCompressCode = false
            ErrorBody = <@ Body.empty |> Element.appendText ["Error!"] :> IHtmlTag @>
            Sections = <@ Map.empty @>
        }

    static member WithHeadTemplate head site =
        { site with HeadTemplate = head } 

    static member WithBodyTemplate body site =
        { site with BodyTemplate = body }      

    static member WithSections sections site =
        { site with Sections = sections }

    static member WithErrorBody body site =
        { site with ErrorBody = body }

    static member WithCompressedCode site =
        { site with CanCompressCode = true }

    static member Start(site : DynamicWebsite) =
        let code = 
            if site.CanCompressCode then
                let raw = FunScript.Compiler.Compiler.Compile(site.Main, components = Helpers.funScriptComponents, noReturn = true, shouldCompress = true)
                let compressor = JavaScriptCompressor()
                compressor.Compress raw
            else FunScript.Compiler.Compiler.Compile(site.Main, components = Helpers.funScriptComponents, noReturn = true)

        let page =
            Html.empty
            |> Element.appendTags [
                
                site.HeadTemplate
                
                site.BodyTemplate
                |> Element.appendTags [
                    Script.empty |> Element.appendText [ code ]
                ]
            ]
        let compiledPage = Compiler.compilePage page
        let sectionHandlers = 
            QuotationEvaluator.Compile site.Sections ()
            |> Map.toList

        Website.At site.Prefix
        |> Website.WithRouteHandlers [

            DynamicRoutes.Site.Crawler.CreateHandler HtmlText (fun () ps ->
                async {
                    let path = ps._escaped_fragment_
                    let segments, queryParams = WebUtility.SplitRelativeUri path
                    let! completedPage =
                        sectionHandlers
                        |> List.foldAsync (fun (acc : IHtmlTag) (elementId, handlers) ->
                            handlers |> List.tryPickAsync (fun f -> f segments queryParams)
                            |> Async.map (fun newHtml -> 
                                newHtml |> Option.fold (fun acc newHtml ->
                                acc |> HtmlTag.replace elementId newHtml) acc)) (page :> IHtmlTag)
                    let compiledPage = Compiler.compilePageUnsafe completedPage
                    return OK compiledPage
                })

            DynamicRoutes.Site.User.CreateHandler HtmlText (fun () ps ->
                async {
                    return OK compiledPage
                })
            |> RequestHandler.cache
        ]
        |> Website.Start