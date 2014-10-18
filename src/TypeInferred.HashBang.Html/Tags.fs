[<AutoOpen; ReflectedDefinition>]
module TypeInferred.HashBang.Html.Tags
#if INTERACTIVE
#r @"..\..\packages\FunScript\lib\net40\FunScript.dll"
#r @"..\..\packages\FunScript\lib\net40\FunScript.Interop.dll"
#r @"..\..\packages\FunScript.TypeScript.Binding.lib\lib\net40\FunScript.TypeScript.Binding.lib.dll"
#r @"..\..\packages\FunScript.TypeScript.Binding.jquery\lib\net40\FunScript.TypeScript.Binding.jquery.dll"
#endif

open System
open FunScript
open FunScript.TypeScript

[<ReflectedDefinition>]
module Disposable =
    
    type DisposeAction(f) =
        interface IDisposable with
            member __.Dispose() = f()

    let empty = new DisposeAction(ignore) :> IDisposable
    
    let by f = new DisposeAction(f) :> IDisposable
    
    let combine (xs:IDisposable[]) = 
        new DisposeAction(fun () -> xs |> Array.iter (fun x -> x.Dispose())) :> IDisposable

type Map<'k,'v when 'k : comparison> = Microsoft.FSharp.Collections.Map<'k,'v>

[<ReflectedDefinition>]
type IHtmlTag =
    abstract Id : string
    abstract Name : string
    abstract Attributes : Map<string, string option>
    abstract Children : TagChild list
    abstract IsVoid : bool
    abstract Initialize : string -> IDisposable

and [<ReflectedDefinition>] TagChild =
    | Text of string
    | Tag of IHtmlTag

[<ReflectedDefinition>]
type HtmlTag<'TElement, 'TTypeScriptElement> =
    {
        Id : string
        Name : string
        Attributes : Map<string, string option>
        Children : TagChild list
        IsVoid : bool
        Initialize : string -> IDisposable
    }

    interface IHtmlTag with
        member tag.Id = tag.Id
        member tag.Name = tag.Name
        member tag.Attributes = tag.Attributes
        member tag.Children = tag.Children
        member tag.IsVoid = tag.IsVoid
        member tag.Initialize v = tag.Initialize v

[<ReflectedDefinition>]
module HtmlTag =
    
    let rec replace id newTag root =
        let rec replaceAux (node : IHtmlTag) =
            if node.Id = id then newTag
            else
                {
                    Id = node.Id
                    Name = node.Name
                    Attributes = node.Attributes
                    Children = 
                        node.Children |> List.map (function
                            | Text _ as x -> x
                            | Tag x -> Tag(replaceAux x))
                    IsVoid = node.IsVoid
                    Initialize = node.Initialize
                } :> IHtmlTag
        replaceAux root

module Unchecked =

    let alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    let private rand = System.Random()

    [<JSEmitInline("Math.floor(Math.random() * {0})")>]
    let random maxExcl =
        rand.Next maxExcl

    let randomId() =
        Array.init 10 (fun _ ->
            alpha.[random alpha.Length].ToString())
        |> String.concat ""

    let nonVoidTag name =
        {
            Id = randomId()
            Name = name
            Attributes = Map.empty
            Children = []
            IsVoid = false
            Initialize = fun _ -> Disposable.empty
        }

    let voidTag name =
        { nonVoidTag name with IsVoid = true }

    let set<'TElement, 'TTypeScriptElement> n v t = 
        { t with Attributes = t.Attributes |> Map.add n (Some v) } : HtmlTag<'TElement, 'TTypeScriptElement>

    let setBool<'TElement, 'TTypeScriptElement> n v t = 
        let v = match v with false -> "false" | true -> "true"
        { t with Attributes = t.Attributes |> Map.add n (Some v) } : HtmlTag<'TElement, 'TTypeScriptElement>

    let setEmpty<'TElement, 'TTypeScriptElement> n t =
        { t with Attributes = t.Attributes |> Map.add n None } : HtmlTag<'TElement, 'TTypeScriptElement>


    [<JSEmitInline("{0}")>]
    let unsafeUnbox<'T> (x:obj) : 'T = failwithf "JavaScript only"

type ElementDir =
    | Rtl
    | Ltr
    | Auto
    member x.Value =
        match x with
        | Rtl -> "rtl"
        | Ltr -> "ltr"
        | Auto -> "auto"

type ElementDraggable =
    | True
    | False
    | Auto
    member x.Value =
        match x with
        | True -> "true"
        | False -> "false"
        | Auto -> "auto"

type ElementDropZone =
    | Copy
    | Move
    | Link
    member x.Value =
        match x with
        | Copy -> "copy"
        | Move -> "move"
        | Link -> "link"

open Unchecked
type INonVoidElement = interface end
type IVoidElement = interface end

[<ReflectedDefinition>]
module Element =
    /// Appends text|tag children to the element
    let append<'TElement, 'TTypeScriptElement when 'TElement :> INonVoidElement> xs (x : HtmlTag<'TElement, 'TTypeScriptElement>) =
        { x with Children = x.Children @ xs }

    /// Appends tag children to the element
    let appendTags xs x = append (xs |> List.map Tag) x

    /// Appends text children to the element
    let appendText xs x = append (xs |> List.map Text) x

    /// Specifies a shortcut key to activate/focus an element
    let accesskey v = set "accesskey" v

    /// Specifies one or more classnames for an element (refers to a class in a style sheet)
    let ``class`` v = set "class" v

    /// Specifies one or more classnames for an element (refers to a class in a style sheet)
    let classes vs = set "class" (vs |> String.concat " ")

    /// Specifies whether the content of an element is editable or not
    let contenteditable v = set "contenteditable" v

    /// Specifies the text direction for the content in an element
    let dir (x : ElementDir) = set "dir" x.Value

    /// Specifies whether an element is draggable or not
    let draggable (x : ElementDraggable) = set "draggable" x.Value

    /// Specifies whether the dragged data is copied, moved, or linked, when dropped
    let dropzone (x : ElementDropZone) = set "dropzone" x.Value

    /// Specifies that an element is not yet, or is no longer, relevant
    let hidden x = setEmpty "hidden" x

    /// Specifies a unique id for an element
    let id v x = { x with Id = v }

    /// Specifies the language of the element's content
    let lang v = set "lang" v

    /// Specifies whether the element is to have its spelling and grammar checked or not
    let spellcheck v = setBool "spellcheck" v

    /// Specifies an inline CSS style for an element
    let style v = set "style" v

    /// Specifies the tabbing order of an element
    let tabindex (v : int) = set "style" (v.ToString())

    /// Specifies extra information about an element
    let title v = set "title" v

    /// Specifies whether the content of an element should be be translated or not
    let translate v = set "translate" v

    /// Finds the element in the document by its identifier
    let getById (x : IHtmlTag) =
        Globals.document.getElementById x.Id

    /// Finds the element in the document by its identifier using JQuery
    let getByJQuery (x : IHtmlTag) =
        Globals.Dollar.Invoke("#" + x.Id)

    /// Adds code that is run when the element is attached to the screen
    let appendSetUpById f x =
        { x with Initialize = fun id -> Disposable.combine [|x.Initialize id; f id|] }

    /// Adds code that is run when the element is attached to the screen
    let appendSetUp<'TElement, 'TTypeScriptElement when 'TTypeScriptElement :> FunScript.TypeScript.HTMLElement> f (x : HtmlTag<'TElement, 'TTypeScriptElement>) =
        appendSetUpById (fun id -> f (unsafeUnbox<'TTypeScriptElement> (Globals.document.getElementById id))) x

    /// Adds code that is run when the element is attached to the screen
    let appendSetUpByJQuery f x =
        appendSetUpById (fun id -> f (Globals.Dollar.Invoke("#" + id))) x

    /// Fires on a mouse click on the element
    let onclick f =
        appendSetUp (fun el ->
            el.onclick <- fun e -> f e; null
            Disposable.by (fun () -> el.onclick <- null))

    /// Fires on a mouse double-click on the element
    let ondblclick f =
        appendSetUp (fun el ->
            el.ondblclick <- fun e -> f e; null
            Disposable.by (fun () -> el.ondblclick <- null))

    /// Script to be run when an element is dragged
    let ondrag f =
        appendSetUp (fun el ->
            el.ondrag <- fun e -> f e; null
            Disposable.by (fun () -> el.ondrag <- null))

    /// Script to be run at the end of a drag operation
    let ondragend f =
        appendSetUp (fun el ->
            el.ondragend <- fun e -> f e; null
            Disposable.by (fun () -> el.ondragend <- null))

    /// Script to be run when an element has been dragged to a valid drop target
    let ondragenter f =
        appendSetUp (fun el ->
            el.ondragenter <- fun e -> f e; null
            Disposable.by (fun () -> el.ondragenter <- null))

    /// Script to be run when an element leaves a valid drop target
    let ondragleave f =
        appendSetUp (fun el ->
            el.ondragleave <- fun e -> f e; null
            Disposable.by (fun () -> el.ondragleave <- null))

    /// Script to be run when an element is being dragged over a valid drop target
    let ondragover f =
        appendSetUp (fun el ->
            el.ondragover <- fun e -> f e; null
            Disposable.by (fun () -> el.ondragover <- null))

    /// Script to be run at the start of a drag operation
    let ondragstart f =
        appendSetUp (fun el ->
            el.ondragstart <- fun e -> f e; null
            Disposable.by (fun () -> el.ondragstart <- null))

    /// Script to be run when dragged element is being dropped
    let ondrop f =
        appendSetUp (fun el ->
            el.ondrop <- fun e -> f e; null
            Disposable.by (fun () -> el.ondrop <- null))

    /// Fires when a mouse button is pressed down on an element
    let onmousedown f =
        appendSetUp (fun el ->
            el.onmousedown <- fun e -> f e; null
            Disposable.by (fun () -> el.onmousedown <- null))

    /// Fires when the mouse pointer moves over an element
    let onmousemove f =
        appendSetUp (fun el ->
            el.onmousemove <- fun e -> f e; null
            Disposable.by (fun () -> el.onmousemove <- null))

    /// Fires when the mouse pointer moves out of an element
    let onmouseout f =
        appendSetUp (fun el ->
            el.onmouseout <- fun e -> f e; null
            Disposable.by (fun () -> el.onmouseout <- null))

    /// Fires when the mouse pointer moves over an element
    let onmouseover f =
        appendSetUp (fun el ->
            el.onmouseover <- fun e -> f e; null
            Disposable.by (fun () -> el.onmouseover <- null))

    /// Fires when a mouse button is released over an element
    let onmouseup f =
        appendSetUp (fun el ->
            el.onmouseup <- fun e -> f e; null
            Disposable.by (fun () -> el.onmouseup <- null))

    /// Script to be run when the mouse wheel is being rotated
    let onmousewheel f =
        appendSetUp (fun el ->
            el.onmousewheel <- fun e -> f e; null
            Disposable.by (fun () -> el.onmousewheel <- null))

    /// Script to be run when an element's scrollbar is being scrolled
    let onscroll f =
        appendSetUp (fun el ->
            el.onscroll <- fun e -> f e; null
            Disposable.by (fun () -> el.onscroll <- null))
            

type IH1Element = inherit INonVoidElement
type H1() =
    static member empty = nonVoidTag "h1" : HtmlTag<IH1Element, FunScript.TypeScript.HTMLHeadingElement>

let h1 classes children = H1.empty |> Element.classes classes |> Element.appendTags children

type IH2Element = inherit INonVoidElement
type H2() =
    static member empty = nonVoidTag "h2" : HtmlTag<IH2Element, FunScript.TypeScript.HTMLHeadingElement>

let h2 classes children = H2.empty |> Element.classes classes |> Element.appendTags children

type IH3Element = inherit INonVoidElement
type H3() =
    static member empty = nonVoidTag "h3" : HtmlTag<IH3Element, FunScript.TypeScript.HTMLHeadingElement>

let h3 classes children = H3.empty |> Element.classes classes |> Element.appendTags children

type IH4Element = inherit INonVoidElement
type H4() =
    static member empty = nonVoidTag "h4" : HtmlTag<IH4Element, FunScript.TypeScript.HTMLHeadingElement>

let h4 classes children = H4.empty |> Element.classes classes |> Element.appendTags children

type IH5Element = inherit INonVoidElement
type H5() =
    static member empty = nonVoidTag "h5" : HtmlTag<IH5Element, FunScript.TypeScript.HTMLHeadingElement>

let h5 classes children = H5.empty |> Element.classes classes |> Element.appendTags children

type IH6Element = inherit INonVoidElement
type H6() =
    static member empty = nonVoidTag "h6" : HtmlTag<IH6Element, FunScript.TypeScript.HTMLHeadingElement>

let h6 classes children = H6.empty |> Element.classes classes |> Element.appendTags children


type Rel =
    | Alternate
    | Author
    | Bookmark
    | Help
    | License
    | Next
    | Nofollow
    | Noreferrer
    | Prefetch
    | Prev
    | Search
    | Tag_
    member x.Value =
        match x with
        | Alternate -> "alternate"
        | Author -> "author"
        | Bookmark -> "bookmark"
        | Help -> "help"
        | License -> "license"
        | Next -> "next"
        | Nofollow -> "nofollow"
        | Noreferrer -> "noreferrer"
        | Prefetch -> "prefetch"
        | Prev -> "prev"
        | Search -> "search"
        | Tag_ -> "tag"

type Target =
    | Blank
    | Parent
    | Self
    | Top
    | Framename of string
    member x.Value =
        match x with
        | Blank -> "_blank"
        | Parent -> "_parent"
        | Self -> "_self"
        | Top -> "_top"
        | Framename framename -> framename

type IAElement = inherit INonVoidElement

type A() =
    static member empty = nonVoidTag "a" : HtmlTag<IAElement, FunScript.TypeScript.HTMLAnchorElement>

    /// Specifies that the target will be downloaded when a user clicks on the  hyperlink
    static member download = set<IAElement, FunScript.TypeScript.HTMLAnchorElement> "download"

    /// Specifies the URL of the page the link goes to
    static member href = set<IAElement, FunScript.TypeScript.HTMLAnchorElement> "href"

    /// Specifies the language of the linked document
    static member hreflang = set<IAElement, FunScript.TypeScript.HTMLAnchorElement> "hreflang"

    /// Specifies what media/device the linked document is optimized for
    static member media = set<IAElement, FunScript.TypeScript.HTMLAnchorElement> "media"

    /// Specifies the relationship between the current document and the linked document
    static member rel (x : Rel) = set<IAElement, FunScript.TypeScript.HTMLAnchorElement> "rel" x.Value

    /// Specifies where to open the linked document
    static member target (x : Target) = set<IAElement, FunScript.TypeScript.HTMLAnchorElement> "target" x.Value

    /// Specifies the media type of the linked document
    static member ``type`` = set<IAElement, FunScript.TypeScript.HTMLAnchorElement> "type"

let a classes children = A.empty |> Element.classes classes |> Element.appendTags children



type IAbbrElement = inherit INonVoidElement

type Abbr() =
    static member empty = nonVoidTag "abbr" : HtmlTag<IAbbrElement, FunScript.TypeScript.HTMLElement>

let abbr classes children = Abbr.empty |> Element.classes classes |> Element.appendTags children



type IAcronymElement = inherit INonVoidElement

type Acronym() =
    static member empty = nonVoidTag "acronym" : HtmlTag<IAcronymElement, FunScript.TypeScript.HTMLElement>

let acronym classes children = Acronym.empty |> Element.classes classes |> Element.appendTags children



type IAddressElement = inherit INonVoidElement

type Address() =
    static member empty = nonVoidTag "address" : HtmlTag<IAddressElement, FunScript.TypeScript.HTMLElement>

let address classes children = Address.empty |> Element.classes classes |> Element.appendTags children



type Align =
    | Left
    | Right
    | Top
    | Bottom
    | Middle
    | Baseline
    member x.Value =
        match x with
        | Left -> "left"
        | Right -> "right"
        | Top -> "top"
        | Bottom -> "bottom"
        | Middle -> "middle"
        | Baseline -> "baseline"

type IAppletElement = inherit INonVoidElement

type Applet() =
    static member empty = nonVoidTag "applet" : HtmlTag<IAppletElement, FunScript.TypeScript.HTMLElement>

    /// Specifies the file name of a Java applet
    static member code = set<IAppletElement, FunScript.TypeScript.HTMLElement> "code"

    /// Specifies a reference to a serialized representation of an  applet
    static member ``object`` = set<IAppletElement, FunScript.TypeScript.HTMLElement> "object"

    /// Specifies the alignment of an applet according to  surrounding elements
    static member align (x : Align) = set<IAppletElement, FunScript.TypeScript.HTMLElement> "align" x.Value

    /// Specifies an alternate text for an applet
    static member alt = set<IAppletElement, FunScript.TypeScript.HTMLElement> "alt"

    /// Specifies the location of an archive file
    static member archive = set<IAppletElement, FunScript.TypeScript.HTMLElement> "archive"

    /// Specifies a relative base URL for applets specified in the  code attribute
    static member codebase = set<IAppletElement, FunScript.TypeScript.HTMLElement> "codebase"

    /// Specifies the height of an applet
    static member height (x : int) = set<IAppletElement, FunScript.TypeScript.HTMLElement> "height" (x.ToString())

    /// Defines the horizontal spacing around an applet
    static member hspace (x : int) = set<IAppletElement, FunScript.TypeScript.HTMLElement> "hspace" (x.ToString())

    /// Defines the name for an applet (to use in scripts)
    static member name = set<IAppletElement, FunScript.TypeScript.HTMLElement> "name"

    /// Defines the vertical spacing around an applet
    static member vspace (x : int) = set<IAppletElement, FunScript.TypeScript.HTMLElement> "vspace" (x.ToString())

    /// Specifies the width of an applet
    static member width (x : int) = set<IAppletElement, FunScript.TypeScript.HTMLElement> "width" (x.ToString())

let applet classes children = Applet.empty |> Element.classes classes |> Element.appendTags children



type Shape =
    | Default
    | Rect
    | Circle
    | Poly
    member x.Value =
        match x with
        | Default -> "default"
        | Rect -> "rect"
        | Circle -> "circle"
        | Poly -> "poly"

type IAreaElement = inherit IVoidElement

type Area() =
    static member empty = voidTag "area" : HtmlTag<IAreaElement, FunScript.TypeScript.HTMLAreaElement>

    /// Specifies an alternate text for the area. Required if the href attribute is present
    static member alt = set<IAreaElement, FunScript.TypeScript.HTMLAreaElement> "alt"

    /// Specifies the coordinates of the area
    static member coords = set<IAreaElement, FunScript.TypeScript.HTMLAreaElement> "coords"

    /// Specifies that the target will be downloaded when a user clicks on the  hyperlink
    static member download = set<IAreaElement, FunScript.TypeScript.HTMLAreaElement> "download"

    /// Specifies the hyperlink target for the area
    static member href = set<IAreaElement, FunScript.TypeScript.HTMLAreaElement> "href"

    /// Specifies the language of the target URL
    static member hreflang = set<IAreaElement, FunScript.TypeScript.HTMLAreaElement> "hreflang"

    /// Specifies what media/device the target URL is optimized for
    static member media = set<IAreaElement, FunScript.TypeScript.HTMLAreaElement> "media"

    /// Specifies the relationship between the current document and  the target URL
    static member rel (x : Rel) = set<IAreaElement, FunScript.TypeScript.HTMLAreaElement> "rel" x.Value

    /// Specifies the shape of the area
    static member shape (x : Shape) = set<IAreaElement, FunScript.TypeScript.HTMLAreaElement> "shape" x.Value

    /// Specifies where to open the target URL
    static member target (x : Target) = set<IAreaElement, FunScript.TypeScript.HTMLAreaElement> "target" x.Value

    /// Specifies the media  type of the target URL
    static member ``type`` = set<IAreaElement, FunScript.TypeScript.HTMLAreaElement> "type"

let area classes = Area.empty |> Element.classes classes



type IArticleElement = inherit INonVoidElement

type Article() =
    static member empty = nonVoidTag "article" : HtmlTag<IArticleElement, FunScript.TypeScript.HTMLElement>

let article classes children = Article.empty |> Element.classes classes |> Element.appendTags children



type IAsideElement = inherit INonVoidElement

type Aside() =
    static member empty = nonVoidTag "aside" : HtmlTag<IAsideElement, FunScript.TypeScript.HTMLElement>

let aside classes children = Aside.empty |> Element.classes classes |> Element.appendTags children



type Preload =
    | Auto
    | Metadata
    | None_
    member x.Value =
        match x with
        | Auto -> "auto"
        | Metadata -> "metadata"
        | None_ -> "none"

type IAudioElement = inherit INonVoidElement

type Audio() =
    static member empty = nonVoidTag "audio" : HtmlTag<IAudioElement, FunScript.TypeScript.HTMLAudioElement>

    /// Specifies that the audio will start playing as soon as it is ready
    static member autoplay = setEmpty<IAudioElement, FunScript.TypeScript.HTMLAudioElement> "autoplay"

    /// Specifies that audio controls should be displayed (such as a play/pause button etc)
    static member controls = setEmpty<IAudioElement, FunScript.TypeScript.HTMLAudioElement> "controls"

    /// Specifies that the audio will start over again, every time it is finished
    static member loop = setEmpty<IAudioElement, FunScript.TypeScript.HTMLAudioElement> "loop"

    /// Specifies that the audio output should be muted
    static member muted = setEmpty<IAudioElement, FunScript.TypeScript.HTMLAudioElement> "muted"

    /// Specifies if and how the author thinks the audio should be loaded when the page loads
    static member preload (x : Preload) = set<IAudioElement, FunScript.TypeScript.HTMLAudioElement> "preload" x.Value

    /// Specifies the URL of the audio file
    static member src = set<IAudioElement, FunScript.TypeScript.HTMLAudioElement> "src"

let audio classes children = Audio.empty |> Element.classes classes |> Element.appendTags children



type IBElement = inherit INonVoidElement

type B() =
    static member empty = nonVoidTag "b" : HtmlTag<IBElement, FunScript.TypeScript.HTMLElement>

let b classes children = B.empty |> Element.classes classes |> Element.appendTags children



type IBaseElement = inherit IVoidElement

type Base() =
    static member empty = voidTag "base" : HtmlTag<IBaseElement, FunScript.TypeScript.HTMLBaseElement>

    /// Specifies the base URL for all relative URLs in the page
    static member href = set<IBaseElement, FunScript.TypeScript.HTMLBaseElement> "href"

    /// Specifies the default target for all hyperlinks and forms in the page
    static member target (x : Target) = set<IBaseElement, FunScript.TypeScript.HTMLBaseElement> "target" x.Value

let baseTag classes = Base.empty |> Element.classes classes



type IBasefontElement = inherit INonVoidElement

type Basefont() =
    static member empty = nonVoidTag "basefont" : HtmlTag<IBasefontElement, FunScript.TypeScript.HTMLElement>

let basefont classes children = Basefont.empty |> Element.classes classes |> Element.appendTags children



type IBdiElement = inherit INonVoidElement

type Bdi() =
    static member empty = nonVoidTag "bdi" : HtmlTag<IBdiElement, FunScript.TypeScript.HTMLElement>

let bdi classes children = Bdi.empty |> Element.classes classes |> Element.appendTags children



type DirType =
    | Ltr
    | Rtl
    member x.Value =
        match x with
        | Ltr -> "ltr"
        | Rtl -> "rtl"

type IBdoElement = inherit INonVoidElement

type Bdo() =
    static member empty = nonVoidTag "bdo" : HtmlTag<IBdoElement, FunScript.TypeScript.HTMLElement>

    /// Required. Specifies the text direction of the text inside the <bdo> element
    static member dir (x : DirType) = set<IBdoElement, FunScript.TypeScript.HTMLElement> "dir" x.Value

let bdo classes children = Bdo.empty |> Element.classes classes |> Element.appendTags children



type IBigElement = inherit INonVoidElement

type Big() =
    static member empty = nonVoidTag "big" : HtmlTag<IBigElement, FunScript.TypeScript.HTMLElement>

let big classes children = Big.empty |> Element.classes classes |> Element.appendTags children



type IBlockquoteElement = inherit INonVoidElement

type Blockquote() =
    static member empty = nonVoidTag "blockquote" : HtmlTag<IBlockquoteElement, FunScript.TypeScript.HTMLQuoteElement>

    /// Specifies the source of the quotation
    static member cite = set<IBlockquoteElement, FunScript.TypeScript.HTMLQuoteElement> "cite"

let blockquote classes children = Blockquote.empty |> Element.classes classes |> Element.appendTags children



type IBodyElement = inherit INonVoidElement

type Body() =
    static member empty = nonVoidTag "body" : HtmlTag<IBodyElement, FunScript.TypeScript.HTMLBodyElement>

let body classes children = Body.empty |> Element.classes classes |> Element.appendTags children



type IBrElement = inherit IVoidElement

type Br() =
    static member empty = voidTag "br" : HtmlTag<IBrElement, FunScript.TypeScript.HTMLBRElement>

let br classes = Br.empty |> Element.classes classes



type Formenctype =
    | Application_x_www_form_urlencoded
    | Multipart_form_data
    | Text_plain
    member x.Value =
        match x with
        | Application_x_www_form_urlencoded -> "application/x-www-form-urlencoded"
        | Multipart_form_data -> "multipart/form-data"
        | Text_plain -> "text/plain"

type Formmethod =
    | Get
    | Post
    member x.Value =
        match x with
        | Get -> "get"
        | Post -> "post"

type Type =
    | Button
    | Reset
    | Submit
    member x.Value =
        match x with
        | Button -> "button"
        | Reset -> "reset"
        | Submit -> "submit"

type IButtonElement = inherit INonVoidElement

type Button() =
    static member empty = nonVoidTag "button" : HtmlTag<IButtonElement, FunScript.TypeScript.HTMLButtonElement>

    /// Specifies that a button should automatically get focus when the page loads
    static member autofocus = setEmpty<IButtonElement, FunScript.TypeScript.HTMLButtonElement> "autofocus"

    /// Specifies that a button should be disabled
    static member disabled = setEmpty<IButtonElement, FunScript.TypeScript.HTMLButtonElement> "disabled"

    /// Specifies one or more forms the button belongs to
    static member form = set<IButtonElement, FunScript.TypeScript.HTMLButtonElement> "form"

    /// Specifies where to send the form-data when a form is submitted. Only for type="submit"
    static member formaction = set<IButtonElement, FunScript.TypeScript.HTMLButtonElement> "formaction"

    /// Specifies how form-data should be encoded before sending it to a server. Only for type="submit"
    static member formenctype (x : Formenctype) = set<IButtonElement, FunScript.TypeScript.HTMLButtonElement> "formenctype" x.Value

    /// Specifies how to send the form-data (which HTTP method to use). Only for type="submit"
    static member formmethod (x : Formmethod) = set<IButtonElement, FunScript.TypeScript.HTMLButtonElement> "formmethod" x.Value

    /// Specifies that the form-data should not be validated on submission. Only for type="submit"
    static member formnovalidate = setEmpty<IButtonElement, FunScript.TypeScript.HTMLButtonElement> "formnovalidate"

    /// Specifies where to display the response after submitting the form. Only for type="submit"
    static member formtarget (x : Target) = set<IButtonElement, FunScript.TypeScript.HTMLButtonElement> "formtarget" x.Value

    /// Specifies a name for the button
    static member name = set<IButtonElement, FunScript.TypeScript.HTMLButtonElement> "name"

    /// Specifies the type of button
    static member ``type`` (x : Type) = set<IButtonElement, FunScript.TypeScript.HTMLButtonElement> "type" x.Value

    /// Specifies an initial value for the button
    static member value = set<IButtonElement, FunScript.TypeScript.HTMLButtonElement> "value"

let button classes children = Button.empty |> Element.classes classes |> Element.appendTags children



type ICanvasElement = inherit INonVoidElement

type Canvas() =
    static member empty = nonVoidTag "canvas" : HtmlTag<ICanvasElement, FunScript.TypeScript.HTMLCanvasElement>

    /// Specifies the height of the canvas
    static member height (x : int) = set<ICanvasElement, FunScript.TypeScript.HTMLCanvasElement> "height" (x.ToString())

    /// Specifies the width of the canvas
    static member width (x : int) = set<ICanvasElement, FunScript.TypeScript.HTMLCanvasElement> "width" (x.ToString())

let canvas classes children = Canvas.empty |> Element.classes classes |> Element.appendTags children



type ICaptionElement = inherit INonVoidElement

type Caption() =
    static member empty = nonVoidTag "caption" : HtmlTag<ICaptionElement, FunScript.TypeScript.HTMLTableCaptionElement>

let caption classes children = Caption.empty |> Element.classes classes |> Element.appendTags children



type ICenterElement = inherit INonVoidElement

type Center() =
    static member empty = nonVoidTag "center" : HtmlTag<ICenterElement, FunScript.TypeScript.HTMLElement>

let center classes children = Center.empty |> Element.classes classes |> Element.appendTags children



type ICiteElement = inherit INonVoidElement

type Cite() =
    static member empty = nonVoidTag "cite" : HtmlTag<ICiteElement, FunScript.TypeScript.HTMLElement>

let cite classes children = Cite.empty |> Element.classes classes |> Element.appendTags children



type ICodeElement = inherit INonVoidElement

type Code() =
    static member empty = nonVoidTag "code" : HtmlTag<ICodeElement, FunScript.TypeScript.HTMLElement>

let code classes children = Code.empty |> Element.classes classes |> Element.appendTags children



type IColElement = inherit IVoidElement

type Col() =
    static member empty = voidTag "col" : HtmlTag<IColElement, FunScript.TypeScript.HTMLTableColElement>

    /// Specifies the number of columns a <col> element should span
    static member span = set<IColElement, FunScript.TypeScript.HTMLTableColElement> "span"

let col classes = Col.empty |> Element.classes classes



type IColgroupElement = inherit INonVoidElement

type Colgroup() =
    static member empty = nonVoidTag "colgroup" : HtmlTag<IColgroupElement, FunScript.TypeScript.HTMLTableColElement>

    /// Specifies the number of columns a column group should span
    static member span = set<IColgroupElement, FunScript.TypeScript.HTMLTableColElement> "span"

let colgroup classes children = Colgroup.empty |> Element.classes classes |> Element.appendTags children



type IDatalistElement = inherit INonVoidElement

type Datalist() =
    static member empty = nonVoidTag "datalist" : HtmlTag<IDatalistElement, FunScript.TypeScript.HTMLDataListElement>

let datalist classes children = Datalist.empty |> Element.classes classes |> Element.appendTags children



type IDdElement = inherit INonVoidElement

type Dd() =
    static member empty = nonVoidTag "dd" : HtmlTag<IDdElement, FunScript.TypeScript.HTMLDDElement>

let dd classes children = Dd.empty |> Element.classes classes |> Element.appendTags children



type IDelElement = inherit INonVoidElement

type Del() =
    static member empty = nonVoidTag "del" : HtmlTag<IDelElement, FunScript.TypeScript.HTMLModElement>

    /// Specifies a URL to a document that explains the reason why the text was deleted
    static member cite = set<IDelElement, FunScript.TypeScript.HTMLModElement> "cite"

    /// Specifies the date and time of when the text was deleted
    static member datetime = set<IDelElement, FunScript.TypeScript.HTMLModElement> "datetime"

let del classes children = Del.empty |> Element.classes classes |> Element.appendTags children



type IDetailsElement = inherit INonVoidElement

type Details() =
    static member empty = nonVoidTag "details" : HtmlTag<IDetailsElement, FunScript.TypeScript.HTMLElement>

    /// Specifies that the details should be visible (open) to the user
    static member ``open`` = setEmpty<IDetailsElement, FunScript.TypeScript.HTMLElement> "open"

let details classes children = Details.empty |> Element.classes classes |> Element.appendTags children



type IDfnElement = inherit INonVoidElement

type Dfn() =
    static member empty = nonVoidTag "dfn" : HtmlTag<IDfnElement, FunScript.TypeScript.HTMLElement>

let dfn classes children = Dfn.empty |> Element.classes classes |> Element.appendTags children



type IDialogElement = inherit INonVoidElement

type Dialog() =
    static member empty = nonVoidTag "dialog" : HtmlTag<IDialogElement, FunScript.TypeScript.HTMLElement>

    /// Specifies that the dialog element is active and that the user can  interact with it
    static member ``open`` = setEmpty<IDialogElement, FunScript.TypeScript.HTMLElement> "open"

let dialog classes children = Dialog.empty |> Element.classes classes |> Element.appendTags children



type IDirElement = inherit INonVoidElement

type Dir() =
    static member empty = nonVoidTag "dir" : HtmlTag<IDirElement, FunScript.TypeScript.HTMLElement>

let dir classes children = Dir.empty |> Element.classes classes |> Element.appendTags children



type IDivElement = inherit INonVoidElement

type Div() =
    static member empty = nonVoidTag "div" : HtmlTag<IDivElement, FunScript.TypeScript.HTMLDivElement>

let div classes children = Div.empty |> Element.classes classes |> Element.appendTags children



type IDlElement = inherit INonVoidElement

type Dl() =
    static member empty = nonVoidTag "dl" : HtmlTag<IDlElement, FunScript.TypeScript.HTMLDListElement>

let dl classes children = Dl.empty |> Element.classes classes |> Element.appendTags children



type IDtElement = inherit INonVoidElement

type Dt() =
    static member empty = nonVoidTag "dt" : HtmlTag<IDtElement, FunScript.TypeScript.HTMLDTElement>

let dt classes children = Dt.empty |> Element.classes classes |> Element.appendTags children



type IEmElement = inherit INonVoidElement

type Em() =
    static member empty = nonVoidTag "em" : HtmlTag<IEmElement, FunScript.TypeScript.HTMLElement>

let em classes children = Em.empty |> Element.classes classes |> Element.appendTags children



type IEmbedElement = inherit IVoidElement

type Embed() =
    static member empty = voidTag "embed" : HtmlTag<IEmbedElement, FunScript.TypeScript.HTMLEmbedElement>

    /// Specifies the height of the embedded content
    static member height (x : int) = set<IEmbedElement, FunScript.TypeScript.HTMLEmbedElement> "height" (x.ToString())

    /// Specifies the address of the external file to embed
    static member src = set<IEmbedElement, FunScript.TypeScript.HTMLEmbedElement> "src"

    /// Specifies the media type of the embedded content
    static member ``type`` = set<IEmbedElement, FunScript.TypeScript.HTMLEmbedElement> "type"

    /// Specifies the width of the embedded content
    static member width (x : int) = set<IEmbedElement, FunScript.TypeScript.HTMLEmbedElement> "width" (x.ToString())

let embed classes = Embed.empty |> Element.classes classes



type IFieldsetElement = inherit INonVoidElement

type Fieldset() =
    static member empty = nonVoidTag "fieldset" : HtmlTag<IFieldsetElement, FunScript.TypeScript.HTMLFieldSetElement>

    /// Specifies that a group of related form elements should be disabled
    static member disabled = setEmpty<IFieldsetElement, FunScript.TypeScript.HTMLFieldSetElement> "disabled"

    /// Specifies one or more forms the fieldset belongs to
    static member form = set<IFieldsetElement, FunScript.TypeScript.HTMLFieldSetElement> "form"

    /// Specifies a name for the fieldset
    static member name = set<IFieldsetElement, FunScript.TypeScript.HTMLFieldSetElement> "name"

let fieldset classes children = Fieldset.empty |> Element.classes classes |> Element.appendTags children



type IFigcaptionElement = inherit INonVoidElement

type Figcaption() =
    static member empty = nonVoidTag "figcaption" : HtmlTag<IFigcaptionElement, FunScript.TypeScript.HTMLElement>

let figcaption classes children = Figcaption.empty |> Element.classes classes |> Element.appendTags children



type IFigureElement = inherit INonVoidElement

type Figure() =
    static member empty = nonVoidTag "figure" : HtmlTag<IFigureElement, FunScript.TypeScript.HTMLElement>

let figure classes children = Figure.empty |> Element.classes classes |> Element.appendTags children



type IFontElement = inherit INonVoidElement

type Font() =
    static member empty = nonVoidTag "font" : HtmlTag<IFontElement, FunScript.TypeScript.HTMLElement>

let font classes children = Font.empty |> Element.classes classes |> Element.appendTags children



type IFooterElement = inherit INonVoidElement

type Footer() =
    static member empty = nonVoidTag "footer" : HtmlTag<IFooterElement, FunScript.TypeScript.HTMLElement>

let footer classes children = Footer.empty |> Element.classes classes |> Element.appendTags children



type Autocomplete =
    | On
    | Off
    member x.Value =
        match x with
        | On -> "on"
        | Off -> "off"

type IFormElementTarget =
    | Blank
    | Self
    | Parent
    | Top
    member x.Value =
        match x with
        | Blank -> "_blank"
        | Self -> "_self"
        | Parent -> "_parent"
        | Top -> "_top"

type IFormElement = inherit INonVoidElement

type Form() =
    static member empty = nonVoidTag "form" : HtmlTag<IFormElement, FunScript.TypeScript.HTMLFormElement>

    /// Specifies the character encodings that are to be used for the form  submission
    static member accept_charset = set<IFormElement, FunScript.TypeScript.HTMLFormElement> "accept-charset"

    /// Specifies where to send the form-data when a form is submitted
    static member action = set<IFormElement, FunScript.TypeScript.HTMLFormElement> "action"

    /// Specifies whether a form should have autocomplete on or off
    static member autocomplete (x : Autocomplete) = set<IFormElement, FunScript.TypeScript.HTMLFormElement> "autocomplete" x.Value

    /// Specifies how the form-data should be encoded when submitting it to the server (only for method="post")
    static member enctype (x : Formenctype) = set<IFormElement, FunScript.TypeScript.HTMLFormElement> "enctype" x.Value

    /// Specifies the HTTP method to use when sending form-data
    static member ``method`` (x : Formmethod) = set<IFormElement, FunScript.TypeScript.HTMLFormElement> "method" x.Value

    /// Specifies the name of a form
    static member name = set<IFormElement, FunScript.TypeScript.HTMLFormElement> "name"

    /// Specifies that the form should not be validated when submitted
    static member novalidate = setEmpty<IFormElement, FunScript.TypeScript.HTMLFormElement> "novalidate"

    /// Specifies where to display the response that is received after submitting the form
    static member target (x : IFormElementTarget) = set<IFormElement, FunScript.TypeScript.HTMLFormElement> "target" x.Value

let form classes children = Form.empty |> Element.classes classes |> Element.appendTags children



type IFrameElement = inherit INonVoidElement

type Frame() =
    static member empty = nonVoidTag "frame" : HtmlTag<IFrameElement, FunScript.TypeScript.HTMLElement>

let frame classes children = Frame.empty |> Element.classes classes |> Element.appendTags children



type IFramesetElement = inherit INonVoidElement

type Frameset() =
    static member empty = nonVoidTag "frameset" : HtmlTag<IFramesetElement, FunScript.TypeScript.HTMLElement>

let frameset classes children = Frameset.empty |> Element.classes classes |> Element.appendTags children



type IHeadElement = inherit INonVoidElement

type Head() =
    static member empty = nonVoidTag "head" : HtmlTag<IHeadElement, FunScript.TypeScript.HTMLHeadElement>

let head classes children = Head.empty |> Element.classes classes |> Element.appendTags children



type IHeaderElement = inherit INonVoidElement

type Header() =
    static member empty = nonVoidTag "header" : HtmlTag<IHeaderElement, FunScript.TypeScript.HTMLElement>

let header classes children = Header.empty |> Element.classes classes |> Element.appendTags children



type IHgroupElement = inherit INonVoidElement

type Hgroup() =
    static member empty = nonVoidTag "hgroup" : HtmlTag<IHgroupElement, FunScript.TypeScript.HTMLElement>

let hgroup classes children = Hgroup.empty |> Element.classes classes |> Element.appendTags children



type IHrElement = inherit IVoidElement

type Hr() =
    static member empty = voidTag "hr" : HtmlTag<IHrElement, FunScript.TypeScript.HTMLHRElement>

let hr classes = Hr.empty |> Element.classes classes



type Xmlns =
    | Http___www_w3_org_1999_xhtml
    member x.Value =
        match x with
        | Http___www_w3_org_1999_xhtml -> "http://www.w3.org/1999/xhtml"

type IHtmlElement = inherit INonVoidElement

type Html() =
    static member empty = nonVoidTag "html" : HtmlTag<IHtmlElement, FunScript.TypeScript.HTMLHtmlElement>

    /// Specifies the address of the document's cache manifest (for offline browsing)
    static member manifest = set<IHtmlElement, FunScript.TypeScript.HTMLHtmlElement> "manifest"

    /// Specifies the XML namespace attribute (If you need your content to conform to XHTML)
    static member xmlns (x : Xmlns) = set<IHtmlElement, FunScript.TypeScript.HTMLHtmlElement> "xmlns" x.Value

let html classes children = Html.empty |> Element.classes classes |> Element.appendTags children



type IIElement = inherit INonVoidElement

type I() =
    static member empty = nonVoidTag "i" : HtmlTag<IIElement, FunScript.TypeScript.HTMLElement>

let i classes children = I.empty |> Element.classes classes |> Element.appendTags children



type Sandbox =
    | Empty
    | Allow_forms
    | Allow_same_origin
    | Allow_scripts
    | Allow_top_navigation
    member x.Value =
        match x with
        | Empty -> ""
        | Allow_forms -> "allow-forms"
        | Allow_same_origin -> "allow-same-origin"
        | Allow_scripts -> "allow-scripts"
        | Allow_top_navigation -> "allow-top-navigation"

type IIframeElement = inherit INonVoidElement

type Iframe() =
    static member empty = nonVoidTag "iframe" : HtmlTag<IIframeElement, FunScript.TypeScript.HTMLIFrameElement>

    /// Specifies the height of an <iframe>
    static member height (x : int) = set<IIframeElement, FunScript.TypeScript.HTMLIFrameElement> "height" (x.ToString())

    /// Specifies the name of an <iframe>
    static member name = set<IIframeElement, FunScript.TypeScript.HTMLIFrameElement> "name"

    /// Enables a set of extra restrictions for the content in the <iframe>
    static member sandbox (x : Sandbox) = set<IIframeElement, FunScript.TypeScript.HTMLIFrameElement> "sandbox" x.Value

    /// Specifies that the <iframe> should look like it is a part of the containing document
    static member seamless = setEmpty<IIframeElement, FunScript.TypeScript.HTMLIFrameElement> "seamless"

    /// Specifies the address of the document to embed in the <iframe>
    static member src = set<IIframeElement, FunScript.TypeScript.HTMLIFrameElement> "src"

    /// Specifies the HTML content of the page to show in the <iframe>
    static member srcdoc = set<IIframeElement, FunScript.TypeScript.HTMLIFrameElement> "srcdoc"

    /// Specifies the width of an <iframe>
    static member width (x : int) = set<IIframeElement, FunScript.TypeScript.HTMLIFrameElement> "width" (x.ToString())

let iframe classes children = Iframe.empty |> Element.classes classes |> Element.appendTags children



type Crossorigin =
    | Anonymous
    | Use_credentials
    member x.Value =
        match x with
        | Anonymous -> "anonymous"
        | Use_credentials -> "use-credentials"

type IImgElement = inherit IVoidElement

type Img() =
    static member empty = voidTag "img" : HtmlTag<IImgElement, FunScript.TypeScript.HTMLImageElement>

    /// Specifies an alternate text for an image
    static member alt = set<IImgElement, FunScript.TypeScript.HTMLImageElement> "alt"

    /// Allow images from third-party sites that allow cross-origin access to be used with canvas
    static member crossorigin (x : Crossorigin) = set<IImgElement, FunScript.TypeScript.HTMLImageElement> "crossorigin" x.Value

    /// Specifies the height of an image
    static member height (x : int) = set<IImgElement, FunScript.TypeScript.HTMLImageElement> "height" (x.ToString())

    /// Specifies an image as a server-side image-map
    static member ismap = setEmpty<IImgElement, FunScript.TypeScript.HTMLImageElement> "ismap"

    /// Specifies the URL of an image
    static member src = set<IImgElement, FunScript.TypeScript.HTMLImageElement> "src"

    /// Specifies an image as a client-side image-map
    static member usemap = set<IImgElement, FunScript.TypeScript.HTMLImageElement> "usemap"

    /// Specifies the width of an image
    static member width (x : int) = set<IImgElement, FunScript.TypeScript.HTMLImageElement> "width" (x.ToString())

let img classes = Img.empty |> Element.classes classes



type Accept =
    | File_extension of string
    | Audio_WildCard
    | Video_WildCard
    | Image_WildCard
    | Media_type of string
    member x.Value =
        match x with
        | File_extension file_extension -> file_extension
        | Audio_WildCard -> "audio/*"
        | Video_WildCard -> "video/*"
        | Image_WildCard -> "image/*"
        | Media_type media_type -> media_type

type Max =
    | Number of string
    | Date
    member x.Value =
        match x with
        | Number number -> number
        | Date -> "date"

type IInputElementType =
    | Button
    | Checkbox
    | Color
    | Date
    | Datetime
    | Datetime_local
    | Email
    | File
    | Hidden
    | Image
    | Month
    | Number
    | Password
    | Radio
    | Range
    | Reset
    | Search
    | Submit
    | Tel
    | Text_
    | Time
    | Url
    | Week
    member x.Value =
        match x with
        | Button -> "button"
        | Checkbox -> "checkbox"
        | Color -> "color"
        | Date -> "date"
        | Datetime -> "datetime"
        | Datetime_local -> "datetime-local"
        | Email -> "email"
        | File -> "file"
        | Hidden -> "hidden"
        | Image -> "image"
        | Month -> "month"
        | Number -> "number"
        | Password -> "password"
        | Radio -> "radio"
        | Range -> "range"
        | Reset -> "reset"
        | Search -> "search"
        | Submit -> "submit"
        | Tel -> "tel"
        | Text_ -> "text"
        | Time -> "time"
        | Url -> "url"
        | Week -> "week"

type IInputElement = inherit IVoidElement

type Input() =
    static member empty = voidTag "input" : HtmlTag<IInputElement, FunScript.TypeScript.HTMLInputElement>

    /// Specifies the types of files that the server accepts (only for type="file")
    static member accept (x : Accept) = set<IInputElement, FunScript.TypeScript.HTMLInputElement> "accept" x.Value

    /// Specifies an alternate text for images (only for type="image")
    static member alt = set<IInputElement, FunScript.TypeScript.HTMLInputElement> "alt"

    /// Specifies whether an <input> element should have autocomplete enabled
    static member autocomplete (x : Autocomplete) = set<IInputElement, FunScript.TypeScript.HTMLInputElement> "autocomplete" x.Value

    /// Specifies that an <input> element should automatically get focus when the page loads
    static member autofocus = setEmpty<IInputElement, FunScript.TypeScript.HTMLInputElement> "autofocus"

    /// Specifies that an <input> element should be pre-selected when the page  loads (for type="checkbox" or type="radio")
    static member ``checked`` = setEmpty<IInputElement, FunScript.TypeScript.HTMLInputElement> "checked"

    /// Specifies that an <input> element should be disabled
    static member disabled = setEmpty<IInputElement, FunScript.TypeScript.HTMLInputElement> "disabled"

    /// Specifies one or more forms the <input> element belongs to
    static member form = set<IInputElement, FunScript.TypeScript.HTMLInputElement> "form"

    /// Specifies the URL of the file that will process the input control when  the form is submitted (for type="submit" and type="image")
    static member formaction = set<IInputElement, FunScript.TypeScript.HTMLInputElement> "formaction"

    /// Specifies how the form-data should be encoded when submitting it to the  server (for type="submit" and type="image")
    static member formenctype (x : Formenctype) = set<IInputElement, FunScript.TypeScript.HTMLInputElement> "formenctype" x.Value

    /// Defines the HTTP method for sending data to the action URL (for type="submit" and type="image")
    static member formmethod (x : Formmethod) = set<IInputElement, FunScript.TypeScript.HTMLInputElement> "formmethod" x.Value

    /// Defines that form elements should not be validated when submitted
    static member formnovalidate = setEmpty<IInputElement, FunScript.TypeScript.HTMLInputElement> "formnovalidate"

    /// Specifies where to display the response that is received after submitting  the form (for type="submit" and type="image")
    static member formtarget (x : Target) = set<IInputElement, FunScript.TypeScript.HTMLInputElement> "formtarget" x.Value

    /// Specifies the height of an <input> element (only for type="image")
    static member height (x : int) = set<IInputElement, FunScript.TypeScript.HTMLInputElement> "height" (x.ToString())

    /// Refers to a <datalist> element that contains pre-defined options for an <input> element
    static member list = set<IInputElement, FunScript.TypeScript.HTMLInputElement> "list"

    /// Specifies the maximum value for an <input> element
    static member max (x : Max) = set<IInputElement, FunScript.TypeScript.HTMLInputElement> "max" x.Value

    /// Specifies the maximum number of characters allowed in an <input> element
    static member maxlength = set<IInputElement, FunScript.TypeScript.HTMLInputElement> "maxlength"

    /// Specifies a minimum value for an <input> element
    static member min (x : Max) = set<IInputElement, FunScript.TypeScript.HTMLInputElement> "min" x.Value

    /// Specifies that a user can enter more than one value in an <input>  element
    static member multiple = setEmpty<IInputElement, FunScript.TypeScript.HTMLInputElement> "multiple"

    /// Specifies the name of an <input> element
    static member name = set<IInputElement, FunScript.TypeScript.HTMLInputElement> "name"

    /// Specifies a regular expression that an <input> element's value is checked against
    static member pattern = set<IInputElement, FunScript.TypeScript.HTMLInputElement> "pattern"

    /// Specifies a short hint that describes the expected value of an <input> element
    static member placeholder = set<IInputElement, FunScript.TypeScript.HTMLInputElement> "placeholder"

    /// Specifies that an input field is read-only
    static member ``readonly`` = setEmpty<IInputElement, FunScript.TypeScript.HTMLInputElement> "readonly"

    /// Specifies that an input field must be filled out before submitting the form
    static member required = setEmpty<IInputElement, FunScript.TypeScript.HTMLInputElement> "required"

    /// Specifies the width, in characters, of an <input> element
    static member size = set<IInputElement, FunScript.TypeScript.HTMLInputElement> "size"

    /// Specifies the URL of the image to use as a submit button (only for  type="image")
    static member src = set<IInputElement, FunScript.TypeScript.HTMLInputElement> "src"

    /// Specifies the legal number intervals for an input field
    static member step = set<IInputElement, FunScript.TypeScript.HTMLInputElement> "step"

    /// Specifies the type <input> element to display
    static member ``type`` (x : IInputElementType) = set<IInputElement, FunScript.TypeScript.HTMLInputElement> "type" x.Value

    /// Specifies the value of an <input> element   
    static member value = set<IInputElement, FunScript.TypeScript.HTMLInputElement> "value"

    /// Specifies the width of an <input> element (only for type="image")
    static member width (x : int) = set<IInputElement, FunScript.TypeScript.HTMLInputElement> "width" (x.ToString())

let input classes = Input.empty |> Element.classes classes



type IInsElement = inherit INonVoidElement

type Ins() =
    static member empty = nonVoidTag "ins" : HtmlTag<IInsElement, FunScript.TypeScript.HTMLModElement>

    /// Specifies a URL to a document that explains the reason why the text was  inserted/changed
    static member cite = set<IInsElement, FunScript.TypeScript.HTMLModElement> "cite"

    /// Specifies the date and time when the text was inserted/changed
    static member datetime = set<IInsElement, FunScript.TypeScript.HTMLModElement> "datetime"

let ins classes children = Ins.empty |> Element.classes classes |> Element.appendTags children



type IKbdElement = inherit INonVoidElement

type Kbd() =
    static member empty = nonVoidTag "kbd" : HtmlTag<IKbdElement, FunScript.TypeScript.HTMLElement>

let kbd classes children = Kbd.empty |> Element.classes classes |> Element.appendTags children



type Keytype =
    | Rsa
    | Dsa
    | Ec
    member x.Value =
        match x with
        | Rsa -> "rsa"
        | Dsa -> "dsa"
        | Ec -> "ec"

type IKeygenElement = inherit IVoidElement

type Keygen() =
    static member empty = voidTag "keygen" : HtmlTag<IKeygenElement, FunScript.TypeScript.HTMLElement>

    /// Specifies that a <keygen> element should automatically get focus when the page loads
    static member autofocus = setEmpty<IKeygenElement, FunScript.TypeScript.HTMLElement> "autofocus"

    /// Specifies that the value of the <keygen> element should be challenged when submitted
    static member challenge = setEmpty<IKeygenElement, FunScript.TypeScript.HTMLElement> "challenge"

    /// Specifies that a <keygen> element should be disabled
    static member disabled = setEmpty<IKeygenElement, FunScript.TypeScript.HTMLElement> "disabled"

    /// Specifies one or more forms the <keygen> element belongs to
    static member form = set<IKeygenElement, FunScript.TypeScript.HTMLElement> "form"

    /// Specifies the security algorithm of the key
    static member keytype (x : Keytype) = set<IKeygenElement, FunScript.TypeScript.HTMLElement> "keytype" x.Value

    /// Defines a name for the <keygen> element
    static member name = set<IKeygenElement, FunScript.TypeScript.HTMLElement> "name"

let keygen classes = Keygen.empty |> Element.classes classes



type ILabelElement = inherit INonVoidElement

type Label() =
    static member empty = nonVoidTag "label" : HtmlTag<ILabelElement, FunScript.TypeScript.HTMLLabelElement>

    /// Specifies which form element a label is bound to
    static member ``for`` = set<ILabelElement, FunScript.TypeScript.HTMLLabelElement> "for"

    /// Specifies one or more forms the label belongs to
    static member form = set<ILabelElement, FunScript.TypeScript.HTMLLabelElement> "form"

let label classes children = Label.empty |> Element.classes classes |> Element.appendTags children



type ILegendElement = inherit INonVoidElement

type Legend() =
    static member empty = nonVoidTag "legend" : HtmlTag<ILegendElement, FunScript.TypeScript.HTMLLegendElement>

let legend classes children = Legend.empty |> Element.classes classes |> Element.appendTags children



type ILiElement = inherit INonVoidElement

type Li() =
    static member empty = nonVoidTag "li" : HtmlTag<ILiElement, FunScript.TypeScript.HTMLLIElement>

    /// Specifies the value of a list item. The following list items will increment  from that number (only for <ol> lists)
    static member value = set<ILiElement, FunScript.TypeScript.HTMLLIElement> "value"

let li classes children = Li.empty |> Element.classes classes |> Element.appendTags children



type ILinkElementRel =
    | Alternate
    | Archives
    | Author
    | Bookmark
    | External
    | First
    | Help
    | Icon
    | Last
    | License
    | Next
    | Nofollow
    | Noreferrer
    | Pingback
    | Prefetch
    | Prev
    | Search
    | Sidebar
    | Stylesheet
    | Tag_
    | Up
    member x.Value =
        match x with
        | Alternate -> "alternate"
        | Archives -> "archives"
        | Author -> "author"
        | Bookmark -> "bookmark"
        | External -> "external"
        | First -> "first"
        | Help -> "help"
        | Icon -> "icon"
        | Last -> "last"
        | License -> "license"
        | Next -> "next"
        | Nofollow -> "nofollow"
        | Noreferrer -> "noreferrer"
        | Pingback -> "pingback"
        | Prefetch -> "prefetch"
        | Prev -> "prev"
        | Search -> "search"
        | Sidebar -> "sidebar"
        | Stylesheet -> "stylesheet"
        | Tag_ -> "tag"
        | Up -> "up"

type Sizes =
    | HeightxWidth of string
    | Any
    member x.Value =
        match x with
        | HeightxWidth heightxWidth -> heightxWidth
        | Any -> "any"

type ILinkElement = inherit IVoidElement

type Link() =
    static member empty = voidTag "link" : HtmlTag<ILinkElement, FunScript.TypeScript.HTMLLIElement>

    /// Specifies the location of the linked document
    static member href = set<ILinkElement, FunScript.TypeScript.HTMLLIElement> "href"

    /// Specifies the language of the text in the linked document
    static member hreflang = set<ILinkElement, FunScript.TypeScript.HTMLLIElement> "hreflang"

    /// Specifies on what device the linked document will be displayed
    static member media = set<ILinkElement, FunScript.TypeScript.HTMLLIElement> "media"

    /// Required. Specifies the relationship between the current document and the linked document
    static member rel (x : ILinkElementRel) = set<ILinkElement, FunScript.TypeScript.HTMLLIElement> "rel" x.Value

    /// Specifies the size of the linked resource. Only for rel="icon"
    static member sizes (x : Sizes) = set<ILinkElement, FunScript.TypeScript.HTMLLIElement> "sizes" x.Value

    /// Specifies the media type of the linked document
    static member ``type`` = set<ILinkElement, FunScript.TypeScript.HTMLLIElement> "type"

let link classes = Link.empty |> Element.classes classes



type IMainElement = inherit INonVoidElement

type Main() =
    static member empty = nonVoidTag "main" : HtmlTag<IMainElement, FunScript.TypeScript.HTMLElement>

let main classes children = Main.empty |> Element.classes classes |> Element.appendTags children



type IMapElementElement = inherit INonVoidElement

type MapElement() =
    static member empty = nonVoidTag "map" : HtmlTag<IMapElementElement, FunScript.TypeScript.HTMLMapElement>

    /// Required. Specifies the name of an image-map
    static member name = set<IMapElementElement, FunScript.TypeScript.HTMLMapElement> "name"

let map classes children = MapElement.empty |> Element.classes classes |> Element.appendTags children



type IMarkElement = inherit INonVoidElement

type Mark() =
    static member empty = nonVoidTag "mark" : HtmlTag<IMarkElement, FunScript.TypeScript.HTMLElement>

let mark classes children = Mark.empty |> Element.classes classes |> Element.appendTags children



type IMenuElementType =
    | Popup
    | Toolbar
    | Context
    member x.Value =
        match x with
        | Popup -> "popup"
        | Toolbar -> "toolbar"
        | Context -> "context"

type IMenuElement = inherit INonVoidElement

type Menu() =
    static member empty = nonVoidTag "menu" : HtmlTag<IMenuElement, FunScript.TypeScript.HTMLMenuElement>

    /// Specifies a visible label for the menu
    static member label = set<IMenuElement, FunScript.TypeScript.HTMLMenuElement> "label"

    /// Specifies which type of menu to display
    static member ``type`` (x : IMenuElementType) = set<IMenuElement, FunScript.TypeScript.HTMLMenuElement> "type" x.Value

let menu classes children = Menu.empty |> Element.classes classes |> Element.appendTags children



type Command =
    | Empty
    member x.Value =
        match x with
        | Empty -> ""

type IMenuitemElementType =
    | Checkbox
    | Command
    | Radio
    member x.Value =
        match x with
        | Checkbox -> "checkbox"
        | Command -> "command"
        | Radio -> "radio"

type IMenuitemElement = inherit INonVoidElement

type Menuitem() =
    static member empty = nonVoidTag "menuitem" : HtmlTag<IMenuitemElement, FunScript.TypeScript.HTMLElement>

    /// Specifies that the command/menu item should be checked when the page loads. Only  for type="radio" or type="checkbox"
    static member ``checked`` = setEmpty<IMenuitemElement, FunScript.TypeScript.HTMLElement> "checked"

    ///  
    static member command (x : Command) = set<IMenuitemElement, FunScript.TypeScript.HTMLElement> "command" x.Value

    /// Marks the command/menu item as being a default command
    static member ``default`` = setEmpty<IMenuitemElement, FunScript.TypeScript.HTMLElement> "default"

    /// Specifies that the command/menu item should be disabled
    static member disabled = setEmpty<IMenuitemElement, FunScript.TypeScript.HTMLElement> "disabled"

    /// Specifies an icon for the command/menu item
    static member icon = set<IMenuitemElement, FunScript.TypeScript.HTMLElement> "icon"

    /// Required. Specifies the name of the command/menu item, as shown to the user
    static member label = set<IMenuitemElement, FunScript.TypeScript.HTMLElement> "label"

    /// Specifies the name of the group of commands that will be toggled when the  command/menu item itself is toggled. Only for type="radio"
    static member radiogroup = set<IMenuitemElement, FunScript.TypeScript.HTMLElement> "radiogroup"

    /// Specifies the type of command/menu item. Default is "command"
    static member ``type`` (x : IMenuitemElementType) = set<IMenuitemElement, FunScript.TypeScript.HTMLElement> "type" x.Value

let menuitem classes children = Menuitem.empty |> Element.classes classes |> Element.appendTags children



type Http_equiv =
    | Content_type
    | Default_style
    | Refresh
    member x.Value =
        match x with
        | Content_type -> "content-type"
        | Default_style -> "default-style"
        | Refresh -> "refresh"

type Name =
    | Application_name
    | Author
    | Description
    | Generator of string
    | Keywords
    member x.Value =
        match x with
        | Application_name -> "application-name"
        | Author -> "author"
        | Description -> "description"
        | Generator generator -> generator
        | Keywords -> "keywords"

type IMetaElement = inherit IVoidElement

type Meta() =
    static member empty = voidTag "meta" : HtmlTag<IMetaElement, FunScript.TypeScript.HTMLMetaElement>

    /// Specifies the character encoding for the HTML document
    static member charset = set<IMetaElement, FunScript.TypeScript.HTMLMetaElement> "charset"

    /// Gives the value associated with the http-equiv or name attribute
    static member content = set<IMetaElement, FunScript.TypeScript.HTMLMetaElement> "content"

    /// Provides an HTTP header for the information/value of the content attribute
    static member http_equiv (x : Http_equiv) = set<IMetaElement, FunScript.TypeScript.HTMLMetaElement> "http-equiv" x.Value

    /// Specifies a name for the metadata
    static member name (x : Name) = set<IMetaElement, FunScript.TypeScript.HTMLMetaElement> "name" x.Value

let meta classes = Meta.empty |> Element.classes classes



type IMeterElement = inherit INonVoidElement

type Meter() =
    static member empty = nonVoidTag "meter" : HtmlTag<IMeterElement, FunScript.TypeScript.HTMLElement>

    /// Specifies one or more forms the <meter> element belongs to
    static member form = set<IMeterElement, FunScript.TypeScript.HTMLElement> "form"

    /// Specifies the range that is considered to be a high value
    static member high = set<IMeterElement, FunScript.TypeScript.HTMLElement> "high"

    /// Specifies the range that is considered to be a low value
    static member low = set<IMeterElement, FunScript.TypeScript.HTMLElement> "low"

    /// Specifies the maximum value of the range
    static member max = set<IMeterElement, FunScript.TypeScript.HTMLElement> "max"

    /// Specifies the minimum value of the range
    static member min = set<IMeterElement, FunScript.TypeScript.HTMLElement> "min"

    /// Specifies what value is the optimal value for the gauge
    static member optimum = set<IMeterElement, FunScript.TypeScript.HTMLElement> "optimum"

    /// Required. Specifies the current value of the gauge
    static member value = set<IMeterElement, FunScript.TypeScript.HTMLElement> "value"

let meter classes children = Meter.empty |> Element.classes classes |> Element.appendTags children



type INavElement = inherit INonVoidElement

type Nav() =
    static member empty = nonVoidTag "nav" : HtmlTag<INavElement, FunScript.TypeScript.HTMLElement>

let nav classes children = Nav.empty |> Element.classes classes |> Element.appendTags children



type INoframesElement = inherit INonVoidElement

type Noframes() =
    static member empty = nonVoidTag "noframes" : HtmlTag<INoframesElement, FunScript.TypeScript.HTMLElement>

let noframes classes children = Noframes.empty |> Element.classes classes |> Element.appendTags children



type INoscriptElement = inherit INonVoidElement

type Noscript() =
    static member empty = nonVoidTag "noscript" : HtmlTag<INoscriptElement, FunScript.TypeScript.HTMLElement>

let noscript classes children = Noscript.empty |> Element.classes classes |> Element.appendTags children



type IObjectElement = inherit INonVoidElement

type Object() =
    static member empty = nonVoidTag "object" : HtmlTag<IObjectElement, FunScript.TypeScript.HTMLObjectElement>

    /// Specifies the URL of the resource to be used by the object
    static member data = set<IObjectElement, FunScript.TypeScript.HTMLObjectElement> "data"

    /// Specifies one or more forms the object belongs to
    static member form = set<IObjectElement, FunScript.TypeScript.HTMLObjectElement> "form"

    /// Specifies the height of the object
    static member height (x : int) = set<IObjectElement, FunScript.TypeScript.HTMLObjectElement> "height" (x.ToString())

    /// Specifies a name for the object
    static member name = set<IObjectElement, FunScript.TypeScript.HTMLObjectElement> "name"

    /// Specifies the media type of data specified in the data attribute
    static member ``type`` = set<IObjectElement, FunScript.TypeScript.HTMLObjectElement> "type"

    /// Specifies the name of a client-side image map to be used with the object
    static member usemap = set<IObjectElement, FunScript.TypeScript.HTMLObjectElement> "usemap"

    /// Specifies the width of the object
    static member width (x : int) = set<IObjectElement, FunScript.TypeScript.HTMLObjectElement> "width" (x.ToString())

let objectTag classes children = Object.empty |> Element.classes classes |> Element.appendTags children



type IOlElementType =
    | Number of float
    | A
    | A1
    | I
    | I1
    member x.Value =
        match x with
        | Number number -> number.ToString()
        | A -> "A"
        | A1 -> "a"
        | I -> "I"
        | I1 -> "i"

type IOlElement = inherit INonVoidElement

type Ol() =
    static member empty = nonVoidTag "ol" : HtmlTag<IOlElement, FunScript.TypeScript.HTMLOListElement>

    /// Specifies that the list order should be descending (9,8,7...)
    static member reversed = setEmpty<IOlElement, FunScript.TypeScript.HTMLOListElement> "reversed"

    /// Specifies the start value of an ordered list
    static member start = set<IOlElement, FunScript.TypeScript.HTMLOListElement> "start"

    /// Specifies the kind of marker to use in the list
    static member ``type`` (x : IOlElementType) = set<IOlElement, FunScript.TypeScript.HTMLOListElement> "type" x.Value

let ol classes children = Ol.empty |> Element.classes classes |> Element.appendTags children



type IOptgroupElement = inherit INonVoidElement

type Optgroup() =
    static member empty = nonVoidTag "optgroup" : HtmlTag<IOptgroupElement, FunScript.TypeScript.HTMLOptGroupElement>

    /// Specifies that an option-group should be disabled
    static member disabled = setEmpty<IOptgroupElement, FunScript.TypeScript.HTMLOptGroupElement> "disabled"

    /// Specifies a label for an option-group
    static member label = set<IOptgroupElement, FunScript.TypeScript.HTMLOptGroupElement> "label"

let optgroup classes children = Optgroup.empty |> Element.classes classes |> Element.appendTags children



type IOptionElement = inherit INonVoidElement

type Option() =
    static member empty = nonVoidTag "option" : HtmlTag<IOptionElement, FunScript.TypeScript.HTMLOptionElement>

    /// Specifies that an option should be disabled
    static member disabled = setEmpty<IOptionElement, FunScript.TypeScript.HTMLOptionElement> "disabled"

    /// Specifies a shorter label for an option
    static member label = set<IOptionElement, FunScript.TypeScript.HTMLOptionElement> "label"

    /// Specifies that an option should be pre-selected when the page loads
    static member selected = setEmpty<IOptionElement, FunScript.TypeScript.HTMLOptionElement> "selected"

    /// Specifies the value to be sent to a server
    static member value = set<IOptionElement, FunScript.TypeScript.HTMLOptionElement> "value"

let option classes children = Option.empty |> Element.classes classes |> Element.appendTags children



type IOutputElement = inherit INonVoidElement

type Output() =
    static member empty = nonVoidTag "output" : HtmlTag<IOutputElement, FunScript.TypeScript.HTMLElement>

    /// Specifies the relationship between the result of the calculation, and the elements used in the calculation
    static member ``for`` = set<IOutputElement, FunScript.TypeScript.HTMLElement> "for"

    /// Specifies one or more forms the output element belongs to
    static member form = set<IOutputElement, FunScript.TypeScript.HTMLElement> "form"

    /// Specifies a name for the output element
    static member name = set<IOutputElement, FunScript.TypeScript.HTMLElement> "name"

let output classes children = Output.empty |> Element.classes classes |> Element.appendTags children



type IPElement = inherit INonVoidElement

type P() =
    static member empty = nonVoidTag "p" : HtmlTag<IPElement, FunScript.TypeScript.HTMLParagraphElement>

let p classes children = P.empty |> Element.classes classes |> Element.appendTags children



type IParamElement = inherit IVoidElement

type Param() =
    static member empty = voidTag "param" : HtmlTag<IParamElement, FunScript.TypeScript.HTMLParamElement>

    /// Specifies the name of a parameter
    static member name = set<IParamElement, FunScript.TypeScript.HTMLParamElement> "name"

    /// Specifies the value of the parameter
    static member value = set<IParamElement, FunScript.TypeScript.HTMLParamElement> "value"

let param classes = Param.empty |> Element.classes classes



type IPreElement = inherit INonVoidElement

type Pre() =
    static member empty = nonVoidTag "pre" : HtmlTag<IPreElement, FunScript.TypeScript.HTMLPreElement>

let pre classes children = Pre.empty |> Element.classes classes |> Element.appendTags children



type IProgressElement = inherit INonVoidElement

type Progress() =
    static member empty = nonVoidTag "progress" : HtmlTag<IProgressElement, FunScript.TypeScript.HTMLProgressElement>

    /// Specifies how much work the task requires in total
    static member max = set<IProgressElement, FunScript.TypeScript.HTMLProgressElement> "max"

    /// Specifies how much of the task has been completed
    static member value = set<IProgressElement, FunScript.TypeScript.HTMLProgressElement> "value"

let progress classes children = Progress.empty |> Element.classes classes |> Element.appendTags children



type IQElement = inherit INonVoidElement

type Q() =
    static member empty = nonVoidTag "q" : HtmlTag<IQElement, FunScript.TypeScript.HTMLQuoteElement>

    /// Specifies the source URL of the quote
    static member cite = set<IQElement, FunScript.TypeScript.HTMLQuoteElement> "cite"

let q classes children = Q.empty |> Element.classes classes |> Element.appendTags children



type IRpElement = inherit INonVoidElement

type Rp() =
    static member empty = nonVoidTag "rp" : HtmlTag<IRpElement, FunScript.TypeScript.HTMLElement>

let rp classes children = Rp.empty |> Element.classes classes |> Element.appendTags children



type IRtElement = inherit INonVoidElement

type Rt() =
    static member empty = nonVoidTag "rt" : HtmlTag<IRtElement, FunScript.TypeScript.HTMLElement>

let rt classes children = Rt.empty |> Element.classes classes |> Element.appendTags children



type IRubyElement = inherit INonVoidElement

type Ruby() =
    static member empty = nonVoidTag "ruby" : HtmlTag<IRubyElement, FunScript.TypeScript.HTMLElement>

let ruby classes children = Ruby.empty |> Element.classes classes |> Element.appendTags children



type ISElement = inherit INonVoidElement

type S() =
    static member empty = nonVoidTag "s" : HtmlTag<ISElement, FunScript.TypeScript.HTMLElement>

let s classes children = S.empty |> Element.classes classes |> Element.appendTags children



type ISampElement = inherit INonVoidElement

type Samp() =
    static member empty = nonVoidTag "samp" : HtmlTag<ISampElement, FunScript.TypeScript.HTMLElement>

let samp classes children = Samp.empty |> Element.classes classes |> Element.appendTags children



type IScriptElement = inherit INonVoidElement

type Script() =
    static member empty = nonVoidTag "script" : HtmlTag<IScriptElement, FunScript.TypeScript.HTMLScriptElement>

    /// Specifies that the script is executed asynchronously (only for external scripts)
    static member ``async`` = setEmpty<IScriptElement, FunScript.TypeScript.HTMLScriptElement> "async"

    /// Specifies the character encoding used in an external script  file
    static member charset = set<IScriptElement, FunScript.TypeScript.HTMLScriptElement> "charset"

    /// Specifies that the script is executed when the page has finished parsing  (only for external scripts)
    static member defer = setEmpty<IScriptElement, FunScript.TypeScript.HTMLScriptElement> "defer"

    /// Specifies the URL of an external script file
    static member src = set<IScriptElement, FunScript.TypeScript.HTMLScriptElement> "src"

    /// Specifies the media type of the script
    static member ``type`` = set<IScriptElement, FunScript.TypeScript.HTMLScriptElement> "type"

let script classes children = Script.empty |> Element.classes classes |> Element.appendTags children



type ISectionElement = inherit INonVoidElement

type Section() =
    static member empty = nonVoidTag "section" : HtmlTag<ISectionElement, FunScript.TypeScript.HTMLElement>

let section classes children = Section.empty |> Element.classes classes |> Element.appendTags children



type ISelectElement = inherit INonVoidElement

type Select() =
    static member empty = nonVoidTag "select" : HtmlTag<ISelectElement, FunScript.TypeScript.HTMLSelectElement>

    /// Specifies that the drop-down list should automatically get focus when  the page loads
    static member autofocus = setEmpty<ISelectElement, FunScript.TypeScript.HTMLSelectElement> "autofocus"

    /// Specifies that a drop-down list should be disabled
    static member disabled = setEmpty<ISelectElement, FunScript.TypeScript.HTMLSelectElement> "disabled"

    /// Defines one or more forms the select field belongs to
    static member form = set<ISelectElement, FunScript.TypeScript.HTMLSelectElement> "form"

    /// Specifies that multiple options can be selected at once
    static member multiple = setEmpty<ISelectElement, FunScript.TypeScript.HTMLSelectElement> "multiple"

    /// Defines a name for the drop-down list
    static member name = set<ISelectElement, FunScript.TypeScript.HTMLSelectElement> "name"

    /// Specifies that the user is required to select a value before submitting the form
    static member required = setEmpty<ISelectElement, FunScript.TypeScript.HTMLSelectElement> "required"

    /// Defines the number of visible options in a drop-down list
    static member size = set<ISelectElement, FunScript.TypeScript.HTMLSelectElement> "size"

let select classes children = Select.empty |> Element.classes classes |> Element.appendTags children



type ISmallElement = inherit INonVoidElement

type Small() =
    static member empty = nonVoidTag "small" : HtmlTag<ISmallElement, FunScript.TypeScript.HTMLElement>

let small classes children = Small.empty |> Element.classes classes |> Element.appendTags children



type ISourceElement = inherit IVoidElement

type Source() =
    static member empty = voidTag "source" : HtmlTag<ISourceElement, FunScript.TypeScript.HTMLSourceElement>

    /// Specifies the type of media resource
    static member media = set<ISourceElement, FunScript.TypeScript.HTMLSourceElement> "media"

    /// Specifies the URL of the media file
    static member src = set<ISourceElement, FunScript.TypeScript.HTMLSourceElement> "src"

    /// Specifies the media type of the media resource
    static member ``type`` = set<ISourceElement, FunScript.TypeScript.HTMLSourceElement> "type"

let source classes = Source.empty |> Element.classes classes



type ISpanElement = inherit INonVoidElement

type Span() =
    static member empty = nonVoidTag "span" : HtmlTag<ISpanElement, FunScript.TypeScript.HTMLSpanElement>

let span classes children = Span.empty |> Element.classes classes |> Element.appendTags children



type IStrikeElement = inherit INonVoidElement

type Strike() =
    static member empty = nonVoidTag "strike" : HtmlTag<IStrikeElement, FunScript.TypeScript.HTMLElement>

let strike classes children = Strike.empty |> Element.classes classes |> Element.appendTags children



type IStrongElement = inherit INonVoidElement

type Strong() =
    static member empty = nonVoidTag "strong" : HtmlTag<IStrongElement, FunScript.TypeScript.HTMLElement>

let strong classes children = Strong.empty |> Element.classes classes |> Element.appendTags children



type IStyleElementType =
    | Text_css
    member x.Value =
        match x with
        | Text_css -> "text/css"

type IStyleElement = inherit INonVoidElement

type Style() =
    static member empty = nonVoidTag "style" : HtmlTag<IStyleElement, FunScript.TypeScript.HTMLStyleElement>

    /// Specifies what media/device the media resource is optimized for
    static member media = set<IStyleElement, FunScript.TypeScript.HTMLStyleElement> "media"

    /// Specifies that the styles only apply to this element's parent element  and that element's child elements
    static member scoped = setEmpty<IStyleElement, FunScript.TypeScript.HTMLStyleElement> "scoped"

    /// Specifies the media type of the <style> tag
    static member ``type`` (x : IStyleElementType) = set<IStyleElement, FunScript.TypeScript.HTMLStyleElement> "type" x.Value

let style classes children = Style.empty |> Element.classes classes |> Element.appendTags children



type ISubElement = inherit INonVoidElement

type Sub() =
    static member empty = nonVoidTag "sub" : HtmlTag<ISubElement, FunScript.TypeScript.HTMLElement>

let sub classes children = Sub.empty |> Element.classes classes |> Element.appendTags children



type ISummaryElement = inherit INonVoidElement

type Summary() =
    static member empty = nonVoidTag "summary" : HtmlTag<ISummaryElement, FunScript.TypeScript.HTMLElement>

let summary classes children = Summary.empty |> Element.classes classes |> Element.appendTags children



type ISupElement = inherit INonVoidElement

type Sup() =
    static member empty = nonVoidTag "sup" : HtmlTag<ISupElement, FunScript.TypeScript.HTMLElement>

let sup classes children = Sup.empty |> Element.classes classes |> Element.appendTags children



type ITableElement = inherit INonVoidElement

type Table() =
    static member empty = nonVoidTag "table" : HtmlTag<ITableElement, FunScript.TypeScript.HTMLTableElement>

    /// Specifies that the table should be sortable
    static member sortable = setEmpty<ITableElement, FunScript.TypeScript.HTMLTableElement> "sortable"

let table classes children = Table.empty |> Element.classes classes |> Element.appendTags children



type ITbodyElement = inherit INonVoidElement

type Tbody() =
    static member empty = nonVoidTag "tbody" : HtmlTag<ITbodyElement, FunScript.TypeScript.HTMLTableSectionElement>

let tbody classes children = Tbody.empty |> Element.classes classes |> Element.appendTags children



type ITdElement = inherit INonVoidElement

type Td() =
    static member empty = nonVoidTag "td" : HtmlTag<ITdElement, FunScript.TypeScript.HTMLTableDataCellElement>

    /// Specifies the number of columns a cell should span
    static member colspan = set<ITdElement, FunScript.TypeScript.HTMLTableDataCellElement> "colspan"

    /// Specifies one or more header cells a cell is related to
    static member headers = set<ITdElement, FunScript.TypeScript.HTMLTableDataCellElement> "headers"

    /// Sets the number of rows a cell should span
    static member rowspan = set<ITdElement, FunScript.TypeScript.HTMLTableDataCellElement> "rowspan"

let td classes children = Td.empty |> Element.classes classes |> Element.appendTags children



type Wrap =
    | Hard
    | Soft
    member x.Value =
        match x with
        | Hard -> "hard"
        | Soft -> "soft"

type ITextareaElement = inherit INonVoidElement

type Textarea() =
    static member empty = nonVoidTag "textarea" : HtmlTag<ITextareaElement, FunScript.TypeScript.HTMLTextAreaElement>

    /// Specifies that a text area should automatically get focus when the page loads
    static member autofocus = setEmpty<ITextareaElement, FunScript.TypeScript.HTMLTextAreaElement> "autofocus"

    /// Specifies the visible width of a text area
    static member cols = set<ITextareaElement, FunScript.TypeScript.HTMLTextAreaElement> "cols"

    /// Specifies that a text area should be disabled
    static member disabled = setEmpty<ITextareaElement, FunScript.TypeScript.HTMLTextAreaElement> "disabled"

    /// Specifies one or more forms the text area belongs to
    static member form = set<ITextareaElement, FunScript.TypeScript.HTMLTextAreaElement> "form"

    /// Specifies the maximum number of characters allowed in the text area
    static member maxlength = set<ITextareaElement, FunScript.TypeScript.HTMLTextAreaElement> "maxlength"

    /// Specifies a name for a text area
    static member name = set<ITextareaElement, FunScript.TypeScript.HTMLTextAreaElement> "name"

    /// Specifies a short hint that describes the expected value of a text area
    static member placeholder = set<ITextareaElement, FunScript.TypeScript.HTMLTextAreaElement> "placeholder"

    /// Specifies that a text area should be read-only
    static member ``readonly`` = setEmpty<ITextareaElement, FunScript.TypeScript.HTMLTextAreaElement> "readonly"

    /// Specifies that a text area is required/must be filled out
    static member required = setEmpty<ITextareaElement, FunScript.TypeScript.HTMLTextAreaElement> "required"

    /// Specifies the visible number of lines in a text area
    static member rows = set<ITextareaElement, FunScript.TypeScript.HTMLTextAreaElement> "rows"

    /// Specifies how the text in a text area is to be wrapped when submitted in a form
    static member wrap (x : Wrap) = set<ITextareaElement, FunScript.TypeScript.HTMLTextAreaElement> "wrap" x.Value

let textarea classes children = Textarea.empty |> Element.classes classes |> Element.appendTags children



type ITfootElement = inherit INonVoidElement

type Tfoot() =
    static member empty = nonVoidTag "tfoot" : HtmlTag<ITfootElement, FunScript.TypeScript.HTMLTableSectionElement>

let tfoot classes children = Tfoot.empty |> Element.classes classes |> Element.appendTags children



type Scope =
    | Col
    | Colgroup
    | Row
    | Rowgroup
    member x.Value =
        match x with
        | Col -> "col"
        | Colgroup -> "colgroup"
        | Row -> "row"
        | Rowgroup -> "rowgroup"

type Sorted =
    | Reversed
    | Number of string
    | Reversed_number of string
    | Number_reversed of string
    member x.Value =
        match x with
        | Reversed -> "reversed"
        | Number number -> number
        | Reversed_number reversed_number -> reversed_number
        | Number_reversed number_reversed -> number_reversed

type IThElement = inherit INonVoidElement

type Th() =
    static member empty = nonVoidTag "th" : HtmlTag<IThElement, FunScript.TypeScript.HTMLTableHeaderCellElement>

    /// Specifies an abbreviated version of the content in a  header cell
    static member abbr = set<IThElement, FunScript.TypeScript.HTMLTableHeaderCellElement> "abbr"

    /// Specifies the number of columns a header cell should span
    static member colspan = set<IThElement, FunScript.TypeScript.HTMLTableHeaderCellElement> "colspan"

    /// Specifies one or more header cells a cell is related to
    static member headers = set<IThElement, FunScript.TypeScript.HTMLTableHeaderCellElement> "headers"

    /// Specifies the number of rows a header cell should span
    static member rowspan = set<IThElement, FunScript.TypeScript.HTMLTableHeaderCellElement> "rowspan"

    /// Specifies whether a header cell is a header for a column, row, or group  of columns or rows
    static member scope (x : Scope) = set<IThElement, FunScript.TypeScript.HTMLTableHeaderCellElement> "scope" x.Value

    /// Defines the sort direction of a column
    static member sorted (x : Sorted) = set<IThElement, FunScript.TypeScript.HTMLTableHeaderCellElement> "sorted" x.Value

let th classes children = Th.empty |> Element.classes classes |> Element.appendTags children



type ITheadElement = inherit INonVoidElement

type Thead() =
    static member empty = nonVoidTag "thead" : HtmlTag<ITheadElement, FunScript.TypeScript.HTMLTableSectionElement>

let thead classes children = Thead.empty |> Element.classes classes |> Element.appendTags children



type ITimeElement = inherit INonVoidElement

type Time() =
    static member empty = nonVoidTag "time" : HtmlTag<ITimeElement, FunScript.TypeScript.HTMLElement>

    /// Represent a machine-readable date/time of the <time> element
    static member datetime = set<ITimeElement, FunScript.TypeScript.HTMLElement> "datetime"

let time classes children = Time.empty |> Element.classes classes |> Element.appendTags children



type ITitleElement = inherit INonVoidElement

type Title() =
    static member empty = nonVoidTag "title" : HtmlTag<ITitleElement, FunScript.TypeScript.HTMLTitleElement>

let title classes children = Title.empty |> Element.classes classes |> Element.appendTags children



type ITrElement = inherit INonVoidElement

type Tr() =
    static member empty = nonVoidTag "tr" : HtmlTag<ITrElement, FunScript.TypeScript.HTMLTableRowElement>

let tr classes children = Tr.empty |> Element.classes classes |> Element.appendTags children



type Kind =
    | Captions
    | Chapters
    | Descriptions
    | Metadata
    | Subtitles
    member x.Value =
        match x with
        | Captions -> "captions"
        | Chapters -> "chapters"
        | Descriptions -> "descriptions"
        | Metadata -> "metadata"
        | Subtitles -> "subtitles"

type ITrackElement = inherit IVoidElement

type Track() =
    static member empty = voidTag "track" : HtmlTag<ITrackElement, FunScript.TypeScript.HTMLTrackElement>

    /// Specifies that the track is to be enabled if the user's preferences do  not indicate that another track would be more appropriate
    static member ``default`` = setEmpty<ITrackElement, FunScript.TypeScript.HTMLTrackElement> "default"

    /// Specifies the kind of text track
    static member kind (x : Kind) = set<ITrackElement, FunScript.TypeScript.HTMLTrackElement> "kind" x.Value

    /// Specifies the title of the text track
    static member label = set<ITrackElement, FunScript.TypeScript.HTMLTrackElement> "label"

    /// Required. Specifies the URL of the track file
    static member src = set<ITrackElement, FunScript.TypeScript.HTMLTrackElement> "src"

    /// Specifies the language of the track text data (required if kind="subtitles")
    static member srclang = set<ITrackElement, FunScript.TypeScript.HTMLTrackElement> "srclang"

let track classes = Track.empty |> Element.classes classes



type ITtElement = inherit INonVoidElement

type Tt() =
    static member empty = nonVoidTag "tt" : HtmlTag<ITtElement, FunScript.TypeScript.HTMLElement>

let tt classes children = Tt.empty |> Element.classes classes |> Element.appendTags children



type IUElement = inherit INonVoidElement

type U() =
    static member empty = nonVoidTag "u" : HtmlTag<IUElement, FunScript.TypeScript.HTMLElement>

let u classes children = U.empty |> Element.classes classes |> Element.appendTags children



type IUlElement = inherit INonVoidElement

type Ul() =
    static member empty = nonVoidTag "ul" : HtmlTag<IUlElement, FunScript.TypeScript.HTMLUListElement>

let ul classes children = Ul.empty |> Element.classes classes |> Element.appendTags children



type IVarElement = inherit INonVoidElement

type Var() =
    static member empty = nonVoidTag "var" : HtmlTag<IVarElement, FunScript.TypeScript.HTMLElement>

let var classes children = Var.empty |> Element.classes classes |> Element.appendTags children



type IVideoElement = inherit INonVoidElement

type Video() =
    static member empty = nonVoidTag "video" : HtmlTag<IVideoElement, FunScript.TypeScript.HTMLVideoElement>

    /// Specifies that the video will start playing as soon as it is ready
    static member autoplay = setEmpty<IVideoElement, FunScript.TypeScript.HTMLVideoElement> "autoplay"

    /// Specifies that video controls should be displayed (such as a play/pause button etc).
    static member controls = setEmpty<IVideoElement, FunScript.TypeScript.HTMLVideoElement> "controls"

    /// Sets the height of the video player
    static member height (x : int) = set<IVideoElement, FunScript.TypeScript.HTMLVideoElement> "height" (x.ToString())

    /// Specifies that the video will start over again, every time it is finished
    static member loop = setEmpty<IVideoElement, FunScript.TypeScript.HTMLVideoElement> "loop"

    /// Specifies that the audio output of the video should be muted
    static member muted = setEmpty<IVideoElement, FunScript.TypeScript.HTMLVideoElement> "muted"

    /// Specifies an image to be shown while the video is downloading, or until the user hits the play button
    static member poster = set<IVideoElement, FunScript.TypeScript.HTMLVideoElement> "poster"

    /// Specifies if and how the author thinks the video should be loaded when the page loads
    static member preload (x : Preload) = set<IVideoElement, FunScript.TypeScript.HTMLVideoElement> "preload" x.Value

    /// Specifies the URL of the video file
    static member src = set<IVideoElement, FunScript.TypeScript.HTMLVideoElement> "src"

    /// Sets the width of the video player
    static member width (x : int) = set<IVideoElement, FunScript.TypeScript.HTMLVideoElement> "width" (x.ToString())

let video classes children = Video.empty |> Element.classes classes |> Element.appendTags children



type IWbrElement = inherit IVoidElement

type Wbr() =
    static member empty = voidTag "wbr" : HtmlTag<IWbrElement, FunScript.TypeScript.HTMLElement>

let wbr classes = Wbr.empty |> Element.classes classes