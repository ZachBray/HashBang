namespace TypeInferred.HashBang.Owin

open Microsoft.FSharp.Quotations
open FunScript
open TypeInferred.HashBang.Routing.Domain
open TypeInferred.HashBang.Html
open Microsoft.FSharp.Linq

type RouteHandler = Path -> QueryParams -> HtmlTag<IBodyElement> option Async

type HashBangOptions =
    {
        IsJavaScriptCompressionEnabled : bool
        PageTemplate : HtmlTag<IBodyElement> * HtmlTag<IScriptElement> -> HtmlTag<IHtmlElement>
        ErrorPageTemplate : (string -> HtmlTag<IBodyElement>) Expr
        PageHandlers : RouteHandler list Expr
        FunScriptComponents : InternalCompiler.CompilerComponent list
    }

    member internal __.CompiledPageHandlers = 
        // TODO: We could try using: Linq.RuntimeHelpers.LeafExpressionConverter.EvaluateQuotation 
        QuotationEvaluator.Compile __.PageHandlers ()