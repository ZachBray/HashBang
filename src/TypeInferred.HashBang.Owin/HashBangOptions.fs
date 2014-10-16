namespace TypeInferred.HashBang

open Microsoft.FSharp.Quotations
open FunScript
open TypeInferred.HashBang.Routing.Domain
open TypeInferred.HashBang.Html
open Microsoft.FSharp.Linq

[<JS>]
type Request = { Path: Path;  QueryParams: QueryParams }

[<JS>]
type IPage =
    abstract TryHandle : Request -> HtmlTag<IBodyElement> option Async

type HashBangOptions =
    {
        /// If this is set the FunScript output will be minimized.
        IsJavaScriptCompressionEnabled : bool
        /// This is the template that is used whenever the server serves a page. 
        /// The template provided should organise all JavaScript and stylesheet imports.
        /// DO NOT use this as a template for individual pages, as it will only be used on the initial
        /// render and not for client (#!) page transitions.
        ServerServedPageTemplate : HtmlTag<IBodyElement> * HtmlTag<IScriptElement> -> HtmlTag<IHtmlElement>
        /// This is the template that is used to display error messages to the user.
        ErrorPageTemplate : (string -> HtmlTag<IBodyElement>) Expr
        /// This is the list of pages that can be rendered by both the client or the server.
        Pages : IPage list
        /// This is the list of services that can be injected into the constructors of pages.
        /// When client-side rendering takes place proxies to these services will be injected instead.
        Services : IService list
        /// This is a function that enables you to add components to the FunScript compiler.
        /// E.g., Components to map the FSharp.Data or Apiary.io type providers.
        FunScriptComponentInjector : InternalCompiler.CompilerComponent list -> InternalCompiler.CompilerComponent list
    }