namespace TypeInferred.HashBang

open Microsoft.FSharp.Quotations
open FunScript
open TypeInferred.HashBang.Routing.Domain
open TypeInferred.HashBang.Html
open Microsoft.FSharp.Linq

[<JS>]
type Request = { Path: Path;  QueryParams: QueryParams }

type RequestHandler = Path -> QueryParams -> HtmlTag<IBodyElement, FunScript.TypeScript.HTMLBodyElement> option Async

[<JS>]
type IPage =
    abstract RequestHandler : RequestHandler

type AdvancedHashbangOptions =
    {
        /// If this is set the FunScript output will be minimized.
        IsJavaScriptCompressionEnabled : bool
        /// This is a function that enables you to add components to the FunScript compiler.
        /// E.g., Components to map the FSharp.Data or Apiary.io type providers.
        FunScriptComponentInjector : InternalCompiler.CompilerComponent list -> InternalCompiler.CompilerComponent list
        /// These are the appenders that will be used to log.
        LogAppenders : Logging.ILogAppender list
        /// This is the maxmimum detail level that will be logged.
        MaxmimumLogDetailLevel : Logging.DetailLevel
    }

    /// These are the default advanced options. You only need to change
    static member Default =
        {
            IsJavaScriptCompressionEnabled = true
            FunScriptComponentInjector = id
            LogAppenders = [Logging.ConsoleLogAppender()]
            MaxmimumLogDetailLevel = Logging.Info
        }

    member internal options.CreateLogger() =
        Logging.Logger(options.MaxmimumLogDetailLevel, options.LogAppenders)

type CompiledFunScript = HtmlTag<IScriptElement, FunScript.TypeScript.HTMLScriptElement>
type PageBody = HtmlTag<IBodyElement, FunScript.TypeScript.HTMLBodyElement>
type HtmlPage = HtmlTag<IHtmlElement, FunScript.TypeScript.HTMLHtmlElement>

type HashBangOptions =
    {
        /// This is the template that is used whenever the server serves a page. 
        /// The template provided should organise all JavaScript and stylesheet imports.
        /// DO NOT use this as a template for individual pages, as it will only be used on the initial
        /// render and not for client (#!) page transitions.
        ServerServedPageTemplate : PageBody * CompiledFunScript -> HtmlPage
        /// This is the template that is used to display error messages to the user.
        ErrorPageTemplate : (string -> PageBody) Expr
        /// This is the list of pages that can be rendered by both the client or the server.
        Pages : IPage list
        /// This is the list of services that can be injected into the constructors of pages.
        /// When client-side rendering takes place proxies to these services will be injected instead.
        Services : IService list
        /// These are advanced options that you don't have to alter from the defaults to get started.
        /// Initially you can just use `AdvancedHashbangOptions.Default`
        Advanced : AdvancedHashbangOptions
    }