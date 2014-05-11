[<AutoOpen; ReflectedDefinition>]
module TypeInferred.HashBang.Html.Tags
#if INTERACTIVE
#r @"..\packages\FunScript.1.1.0.25\lib\net40\FunScript.dll"
#r @"..\packages\FunScript.1.1.0.25\lib\net40\FunScript.Interop.dll"
#r @"..\packages\FunScript.TypeScript.Binding.lib.1.1.0.13\lib\net40\FunScript.TypeScript.Binding.lib.dll"
#r @"..\packages\FunScript.TypeScript.Binding.jquery.1.1.0.13\lib\net40\FunScript.TypeScript.Binding.jquery.dll"
#endif

open FunScript
open FunScript.TypeScript

type Map<'k,'v when 'k : comparison> = Microsoft.FSharp.Collections.Map<'k,'v>

type IHtmlTag =
    abstract Id : string
    abstract Name : string
    abstract Attributes : Map<string, string option>
    abstract Children : TagChild list
    abstract CanClose : bool
    abstract Initialize : string -> unit

and TagChild =
    | Text of string
    | Tag of IHtmlTag

type HtmlTag<'a> =
    {
        Id : string
        Name : string
        Attributes : Map<string, string option>
        Children : TagChild list
        CanClose : bool
        Initialize : string -> unit
    }

    interface IHtmlTag with
        member tag.Id = tag.Id
        member tag.Name = tag.Name
        member tag.Attributes = tag.Attributes
        member tag.Children = tag.Children
        member tag.CanClose = tag.CanClose
        member tag.Initialize v = tag.Initialize v

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
                    CanClose = node.CanClose
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

    let tag name =
        {
            Id = randomId()
            Name = name
            Attributes = Map.empty
            Children = []
            CanClose = true
            Initialize = ignore
        }

    let unclosedTag name =
        { tag name with CanClose = false }

    let set<'a> n v t = 
        { t with Attributes = t.Attributes |> Map.add n (Some v) } : HtmlTag<'a>

    let setBool<'a> n v t = 
        let v = match v with false -> "false" | true -> "true"
        { t with Attributes = t.Attributes |> Map.add n (Some v) } : HtmlTag<'a>

    let setEmpty<'a> n t =
        { t with Attributes = t.Attributes |> Map.add n None } : HtmlTag<'a>


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
type IClosedElement = interface end
type IUnclosedElement = interface end

module Element =
    /// Appends text|tag children to the element
    let append<'a when 'a :> IClosedElement> xs (x : HtmlTag<'a>) =
        { x with Children = x.Children @ xs }

    /// Appends tag children to the element
    let appendTags<'a when 'a :> IClosedElement> xs (x : HtmlTag<'a>) =
        { x with Children = x.Children @ (xs |> List.map Tag) }

    /// Appends text children to the element
    let appendText<'a when 'a :> IClosedElement> xs (x : HtmlTag<'a>) =
        { x with Children = x.Children @ (xs |> List.map Text) }

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
        { x with Initialize = fun id -> f id; x.Initialize id }

    /// Adds code that is run when the element is attached to the screen
    let appendSetUp f x =
        appendSetUpById (fun id -> f (Globals.document.getElementById id)) x

    /// Adds code that is run when the element is attached to the screen
    let appendSetUpByJQuery f x =
        appendSetUpById (fun id -> f (Globals.Dollar.Invoke("#" + id))) x

    /// Fires on a mouse click on the element
    let onclick f =
        appendSetUp (fun el ->
            el.onclick <- fun e -> f e; null)

    /// Fires on a mouse double-click on the element
    let ondblclick f =
        appendSetUp (fun el ->
            el.ondblclick <- fun e -> f e; null)

    /// Script to be run when an element is dragged
    let ondrag f =
        appendSetUp (fun el ->
            el.ondrag <- fun e -> f e; null)

    /// Script to be run at the end of a drag operation
    let ondragend f =
        appendSetUp (fun el ->
            el.ondragend <- fun e -> f e; null)

    /// Script to be run when an element has been dragged to a valid drop target
    let ondragenter f =
        appendSetUp (fun el ->
            el.ondragenter <- fun e -> f e; null)

    /// Script to be run when an element leaves a valid drop target
    let ondragleave f =
        appendSetUp (fun el ->
            el.ondragleave <- fun e -> f e; null)

    /// Script to be run when an element is being dragged over a valid drop target
    let ondragover f =
        appendSetUp (fun el ->
            el.ondragover <- fun e -> f e; null)

    /// Script to be run at the start of a drag operation
    let ondragstart f =
        appendSetUp (fun el ->
            el.ondragstart <- fun e -> f e; null)

    /// Script to be run when dragged element is being dropped
    let ondrop f =
        appendSetUp (fun el ->
            el.ondrop <- fun e -> f e; null)

    /// Fires when a mouse button is pressed down on an element
    let onmousedown f =
        appendSetUp (fun el ->
            el.onmousedown <- fun e -> f e; null)

    /// Fires when the mouse pointer moves over an element
    let onmousemove f =
        appendSetUp (fun el ->
            el.onmousemove <- fun e -> f e; null)

    /// Fires when the mouse pointer moves out of an element
    let onmouseout f =
        appendSetUp (fun el ->
            el.onmouseout <- fun e -> f e; null)

    /// Fires when the mouse pointer moves over an element
    let onmouseover f =
        appendSetUp (fun el ->
            el.onmouseover <- fun e -> f e; null)

    /// Fires when a mouse button is released over an element
    let onmouseup f =
        appendSetUp (fun el ->
            el.onmouseup <- fun e -> f e; null)

    /// Script to be run when the mouse wheel is being rotated
    let onmousewheel f =
        appendSetUp (fun el ->
            el.onmousewheel <- fun e -> f e; null)

    /// Script to be run when an element's scrollbar is being scrolled
    let onscroll f =
        appendSetUp (fun el ->
            el.onscroll <- fun e -> f e; null)


type IH1Element = inherit IClosedElement
type H1() =
    static member empty = tag "h1" : HtmlTag<IH1Element>

type IH2Element = inherit IClosedElement
type H2() =
    static member empty = tag "h2" : HtmlTag<IH2Element>

type IH3Element = inherit IClosedElement
type H3() =
    static member empty = tag "h3" : HtmlTag<IH3Element>

type IH4Element = inherit IClosedElement
type H4() =
    static member empty = tag "h4" : HtmlTag<IH4Element>

type IH5Element = inherit IClosedElement
type H5() =
    static member empty = tag "h5" : HtmlTag<IH5Element>

type IH6Element = inherit IClosedElement
type H6() =
    static member empty = tag "h6" : HtmlTag<IH6Element>


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
    | TagCase
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
        | TagCase -> "tag"

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

type IAElement = inherit IClosedElement

type A() =
    static member empty = tag "a" : HtmlTag<IAElement>

    /// Specifies the hyperlink target to be downloaded
    static member download = set<IAElement> "download"

    /// Specifies the URL of the page the link goes to
    static member href = set<IAElement> "href"

    /// Specifies the language of the linked document
    static member hreflang = set<IAElement> "hreflang"

    /// Specifies what media/device the linked document is optimized for
    static member media = set<IAElement> "media"

    /// Specifies the relationship between the current document and the linked document
    static member rel (x : Rel) = set<IAElement> "rel" x.Value

    /// Specifies where to open the linked document
    static member target (x : Target) = set<IAElement> "target" x.Value

    /// Specifies the MIME  type of the linked document
    static member ``type`` = set<IAElement> "type"



type IAbbrElement = inherit IClosedElement

type Abbr() =
    static member empty = tag "abbr" : HtmlTag<IAbbrElement>



type DirType =
    | Rtl
    | Ltr
    member x.Value =
        match x with
        | Rtl -> "rtl"
        | Ltr -> "ltr"

type IAcronymElement = inherit IClosedElement

type Acronym() =
    static member empty = tag "acronym" : HtmlTag<IAcronymElement>

    /// Specifies a classname for an element
    static member ``class`` = set<IAcronymElement> "class"

    /// Specifies the text direction  for the content in an element
    static member dir (x : DirType) = set<IAcronymElement> "dir" x.Value

    /// Specifies a unique id for an element
    static member id = set<IAcronymElement> "id"

    /// Specifies a language code for the content in an element
    static member lang = set<IAcronymElement> "lang"

    /// Specifies an inline style for an element
    static member style = set<IAcronymElement> "style"

    /// Specifies extra information about an element
    static member title = set<IAcronymElement> "title"

    /// Specifies a language code for the content in an element, in  XHTML documents
    static member xml_lang = set<IAcronymElement> "xml:lang"



type IAddressElement = inherit IClosedElement

type Address() =
    static member empty = tag "address" : HtmlTag<IAddressElement>



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

type IAppletElement = inherit IClosedElement

type Applet() =
    static member empty = tag "applet" : HtmlTag<IAppletElement>

    static member appendSetUp f (x : HtmlTag<IAppletElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLAppletElement> el)) x

    static member getById (x : HtmlTag<IAppletElement>) =
        unbox<HTMLAppletElement> (Globals.document.getElementById x.Id)

    /// Specifies the file name of a Java applet
    static member code = set<IAppletElement> "code"

    /// Specifies a reference to a serialized representation of an  applet
    static member ``object`` = set<IAppletElement> "object"

    /// Specifies the alignment of an applet according to  surrounding elements
    static member align (x : Align) = set<IAppletElement> "align" x.Value

    /// Specifies an alternate text for an applet
    static member alt = set<IAppletElement> "alt"

    /// Specifies the location of an archive file
    static member archive = set<IAppletElement> "archive"

    /// Specifies a relative base URL for applets specified in the  code attribute
    static member codebase = set<IAppletElement> "codebase"

    /// Specifies the height of an applet
    static member height (x : int) = set<IAppletElement> "height" (x.ToString())

    /// Defines the horizontal spacing around an applet
    static member hspace (x : int) = set<IAppletElement> "hspace" (x.ToString())

    /// Defines the name for an applet (to use in scripts)
    static member name = set<IAppletElement> "name"

    /// Defines the vertical spacing around an applet
    static member vspace (x : int) = set<IAppletElement> "vspace" (x.ToString())

    /// Specifies the width of an applet
    static member width (x : int) = set<IAppletElement> "width" (x.ToString())

    /// Specifies a classname for an element
    static member ``class`` = set<IAppletElement> "class"

    /// Specifies a unique id for an element
    static member id = set<IAppletElement> "id"

    /// Specifies an inline style for an element
    static member style = set<IAppletElement> "style"

    /// Specifies extra information about an element
    static member title = set<IAppletElement> "title"



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

type IAreaElement = inherit IUnclosedElement

type Area() =
    static member empty = unclosedTag "area" : HtmlTag<IAreaElement>

    static member appendSetUp f (x : HtmlTag<IAreaElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLAreaElement> el)) x

    static member getById (x : HtmlTag<IAreaElement>) =
        unbox<HTMLAreaElement> (Globals.document.getElementById x.Id)

    /// Specifies an alternate text for the area. Required if the href attribute is present
    static member alt = set<IAreaElement> "alt"

    /// Specifies the coordinates of the area
    static member coords = set<IAreaElement> "coords"

    /// Specifies the hyperlink target to be downloaded
    static member download = set<IAreaElement> "download"

    /// Specifies the hyperlink target for the area
    static member href = set<IAreaElement> "href"

    /// Specifies the language of the target URL
    static member hreflang = set<IAreaElement> "hreflang"

    /// Specifies what media/device the target URL is optimized for
    static member media = set<IAreaElement> "media"

    /// Specifies the relationship between the current document and  the target URL
    static member rel (x : Rel) = set<IAreaElement> "rel" x.Value

    /// Specifies the shape of the area
    static member shape (x : Shape) = set<IAreaElement> "shape" x.Value

    /// Specifies where to open the target URL
    static member target (x : Target) = set<IAreaElement> "target" x.Value

    /// Specifies the MIME  type of the target URL
    static member ``type`` = set<IAreaElement> "type"



type IArticleElement = inherit IClosedElement

type Article() =
    static member empty = tag "article" : HtmlTag<IArticleElement>



type IAsideElement = inherit IClosedElement

type Aside() =
    static member empty = tag "aside" : HtmlTag<IAsideElement>



type Preload =
    | Auto
    | Metadata
    | NoneCase
    member x.Value =
        match x with
        | Auto -> "auto"
        | Metadata -> "metadata"
        | NoneCase -> "none"

type IAudioElement = inherit IClosedElement

type Audio() =
    static member empty = tag "audio" : HtmlTag<IAudioElement>

    static member appendSetUp f (x : HtmlTag<IAudioElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLAudioElement> el)) x

    static member getById (x : HtmlTag<IAudioElement>) =
        unbox<HTMLAudioElement> (Globals.document.getElementById x.Id)

    /// Specifies that the audio will start playing as soon as it is ready
    static member autoplay = setEmpty<IAudioElement> "autoplay"

    /// Specifies that audio controls should be displayed (such as a play/pause  button etc).
    static member controls = setEmpty<IAudioElement> "controls"

    /// Specifies that the audio will start over again, every time it  is finished
    static member loop = setEmpty<IAudioElement> "loop"

    /// Specifies that the audio output should be muted
    static member muted = setEmpty<IAudioElement> "muted"

    /// Specifies if and how the author thinks the audio should be loaded when the page loads
    static member preload (x : Preload) = set<IAudioElement> "preload" x.Value

    ///   Specifies the URL of the audio file
    static member src = set<IAudioElement> "src"



type IBElement = inherit IClosedElement

type B() =
    static member empty = tag "b" : HtmlTag<IBElement>



type IBaseElement = inherit IUnclosedElement

type Base() =
    static member empty = unclosedTag "base" : HtmlTag<IBaseElement>

    static member appendSetUp f (x : HtmlTag<IBaseElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLBaseElement> el)) x

    static member getById (x : HtmlTag<IBaseElement>) =
        unbox<HTMLBaseElement> (Globals.document.getElementById x.Id)

    /// Specifies the base URL for all relative URLs in the page
    static member href = set<IBaseElement> "href"

    /// Specifies the default target for all hyperlinks and forms in the page
    static member target (x : Target) = set<IBaseElement> "target" x.Value



type IBasefontElement = inherit IUnclosedElement

type Basefont() =
    static member empty = unclosedTag "basefont" : HtmlTag<IBasefontElement>

    static member appendSetUp f (x : HtmlTag<IBasefontElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLBaseFontElement> el)) x

    static member getById (x : HtmlTag<IBasefontElement>) =
        unbox<HTMLBaseFontElement> (Globals.document.getElementById x.Id)

    /// Specifies a classname for an element
    static member ``class`` = set<IBasefontElement> "class"

    /// Specifies the text direction  for the content in an element
    static member dir (x : DirType) = set<IBasefontElement> "dir" x.Value

    /// Specifies a unique id for an element
    static member id = set<IBasefontElement> "id"

    /// Specifies a language code for the content in an element
    static member lang = set<IBasefontElement> "lang"

    /// Specifies an inline style for an element
    static member style = set<IBasefontElement> "style"

    /// Specifies extra information about an element
    static member title = set<IBasefontElement> "title"



type IBdiElement = inherit IClosedElement

type Bdi() =
    static member empty = tag "bdi" : HtmlTag<IBdiElement>



type IBdoElement = inherit IClosedElement

type Bdo() =
    static member empty = tag "bdo" : HtmlTag<IBdoElement>

    /// Required. Specifies the text direction of the text inside the <bdo> element
    static member dir (x : DirType) = set<IBdoElement> "dir" x.Value



type IBigElement = inherit IClosedElement

type Big() =
    static member empty = tag "big" : HtmlTag<IBigElement>

    /// Specifies a classname for an element
    static member ``class`` = set<IBigElement> "class"

    /// Specifies the text direction for the content in an element
    static member dir (x : DirType) = set<IBigElement> "dir" x.Value

    /// Specifies a unique id for an element
    static member id = set<IBigElement> "id"

    /// Specifies a language code for the content in an element
    static member lang = set<IBigElement> "lang"

    /// Specifies an inline style for an element
    static member style = set<IBigElement> "style"

    /// Specifies extra information about an element
    static member title = set<IBigElement> "title"

    /// Specifies a language code for the content in an element, in  XHTML documents
    static member xml_lang = set<IBigElement> "xml:lang"



type IBlockquoteElement = inherit IClosedElement

type Blockquote() =
    static member empty = tag "blockquote" : HtmlTag<IBlockquoteElement>

    /// Specifies the source of the quotation
    static member cite = set<IBlockquoteElement> "cite"



type IBodyElement = inherit IClosedElement

type Body() =
    static member empty = tag "body" : HtmlTag<IBodyElement>

    static member appendSetUp f (x : HtmlTag<IBodyElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLBodyElement> el)) x

    static member getById (x : HtmlTag<IBodyElement>) =
        unbox<HTMLBodyElement> (Globals.document.getElementById x.Id)



type IBrElement = inherit IUnclosedElement

type Br() =
    static member empty = unclosedTag "br" : HtmlTag<IBrElement>

    static member appendSetUp f (x : HtmlTag<IBrElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLBRElement> el)) x

    static member getById (x : HtmlTag<IBrElement>) =
        unbox<HTMLBRElement> (Globals.document.getElementById x.Id)



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
        | Submit -> "submit&nbsp;"

type IButtonElement = inherit IClosedElement

type Button() =
    static member empty = tag "button" : HtmlTag<IButtonElement>

    static member appendSetUp f (x : HtmlTag<IButtonElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLButtonElement> el)) x

    static member getById (x : HtmlTag<IButtonElement>) =
        unbox<HTMLButtonElement> (Globals.document.getElementById x.Id)

    /// Specifies that a button should automatically get focus when the page loads
    static member autofocus = setEmpty<IButtonElement> "autofocus"

    /// Specifies that a button should be disabled
    static member disabled = setEmpty<IButtonElement> "disabled"

    /// Specifies one or more forms the button belongs to
    static member form = set<IButtonElement> "form"

    /// Specifies where to send the form-data when a form is submitted. Only for type="submit"
    static member formaction = set<IButtonElement> "formaction"

    /// Specifies how form-data should be encoded before sending it to a server. Only for type="submit"
    static member formenctype (x : Formenctype) = set<IButtonElement> "formenctype" x.Value

    /// Specifies how to send the form-data (which HTTP method to use). Only for type="submit"
    static member formmethod (x : Formmethod) = set<IButtonElement> "formmethod" x.Value

    /// Specifies that the form-data should not be validated on submission. Only for type="submit"
    static member formnovalidate = setEmpty<IButtonElement> "formnovalidate"

    /// Specifies where to display the response after submitting the form. Only for type="submit"
    static member formtarget (x : Target) = set<IButtonElement> "formtarget" x.Value

    /// Specifies a name for the button
    static member name = set<IButtonElement> "name"

    /// Specifies the type of button
    static member ``type`` (x : Type) = set<IButtonElement> "type" x.Value

    /// Specifies an initial value for the button
    static member value = set<IButtonElement> "value"



type ICanvasElement = inherit IClosedElement

type Canvas() =
    static member empty = tag "canvas" : HtmlTag<ICanvasElement>

    static member appendSetUp f (x : HtmlTag<ICanvasElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLCanvasElement> el)) x

    static member getById (x : HtmlTag<ICanvasElement>) =
        unbox<HTMLCanvasElement> (Globals.document.getElementById x.Id)

    /// Specifies the height of the canvas
    static member height (x : int) = set<ICanvasElement> "height" (x.ToString())

    /// Specifies the width of the canvas
    static member width (x : int) = set<ICanvasElement> "width" (x.ToString())



type ICaptionElement = inherit IUnclosedElement

type Caption() =
    static member empty = unclosedTag "caption" : HtmlTag<ICaptionElement>



type ICenterElement = inherit IClosedElement

type Center() =
    static member empty = tag "center" : HtmlTag<ICenterElement>

    /// Specifies a classname for an element
    static member ``class`` = set<ICenterElement> "class"

    /// Specifies the text direction  for the content in an element
    static member dir (x : DirType) = set<ICenterElement> "dir" x.Value

    /// Specifies a unique id for an element
    static member id = set<ICenterElement> "id"

    /// Specifies a language code for the content in an element
    static member lang = set<ICenterElement> "lang"

    /// Specifies an inline style for an element
    static member style = set<ICenterElement> "style"

    /// Specifies extra information about an element
    static member title = set<ICenterElement> "title"



type ICiteElement = inherit IClosedElement

type Cite() =
    static member empty = tag "cite" : HtmlTag<ICiteElement>



type ICodeElement = inherit IClosedElement

type Code() =
    static member empty = tag "code" : HtmlTag<ICodeElement>



type IColElement = inherit IUnclosedElement

type Col() =
    static member empty = unclosedTag "col" : HtmlTag<IColElement>

    /// Specifies the number of columns a <col> element should span
    static member span = set<IColElement> "span"



type IColgroupElement = inherit IClosedElement

type Colgroup() =
    static member empty = tag "colgroup" : HtmlTag<IColgroupElement>

    /// Specifies the number of columns a column group should span
    static member span = set<IColgroupElement> "span"



type ICommandElementType =
    | Checkbox
    | Command
    | Radio
    member x.Value =
        match x with
        | Checkbox -> "checkbox"
        | Command -> "command"
        | Radio -> "radio"

type ICommandElement = inherit IClosedElement

type Command() =
    static member empty = tag "command" : HtmlTag<ICommandElement>

    /// Specifies that the command should be checked when the page loads. Only  for type="radio" or type="checkbox"
    static member ``checked`` = setEmpty<ICommandElement> "checked"

    /// Specifies that the command should be disabled
    static member disabled = setEmpty<ICommandElement> "disabled"

    /// Specifies an image that represents the command
    static member icon = set<ICommandElement> "icon"

    /// Required. Specifies the name of the command, as shown to the user
    static member label = set<ICommandElement> "label"

    /// Specifies the name of the group of commands that will be toggled when the command itself is toggled.  Only for type="radio"
    static member radiogroup = set<ICommandElement> "radiogroup"

    /// Specifies the type of command
    static member ``type`` (x : ICommandElementType) = set<ICommandElement> "type" x.Value



type IDatalistElement = inherit IClosedElement

type Datalist() =
    static member empty = tag "datalist" : HtmlTag<IDatalistElement>

    static member appendSetUp f (x : HtmlTag<IDatalistElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLDataListElement> el)) x

    static member getById (x : HtmlTag<IDatalistElement>) =
        unbox<HTMLDataListElement> (Globals.document.getElementById x.Id)



type IDdElement = inherit IClosedElement

type Dd() =
    static member empty = tag "dd" : HtmlTag<IDdElement>

    static member appendSetUp f (x : HtmlTag<IDdElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLDDElement> el)) x

    static member getById (x : HtmlTag<IDdElement>) =
        unbox<HTMLDDElement> (Globals.document.getElementById x.Id)



type IDelElement = inherit IClosedElement

type Del() =
    static member empty = tag "del" : HtmlTag<IDelElement>

    /// Specifies a URL to a document that explains the reason why the text was deleted
    static member cite = set<IDelElement> "cite"

    /// Specifies the date and time of when the text was deleted
    static member datetime = set<IDelElement> "datetime"



type IDetailsElement = inherit IClosedElement

type Details() =
    static member empty = tag "details" : HtmlTag<IDetailsElement>

    /// Specifies that the details should be visible (open) to the user
    static member ``open`` = setEmpty<IDetailsElement> "open"



type IDfnElement = inherit IClosedElement

type Dfn() =
    static member empty = tag "dfn" : HtmlTag<IDfnElement>



type IDialogElement = inherit IClosedElement

type Dialog() =
    static member empty = tag "dialog" : HtmlTag<IDialogElement>

    /// Specifies that the dialog element is active and that the user can  interact with it
    static member ``open`` = setEmpty<IDialogElement> "open"



type IDirElement = inherit IClosedElement

type Dir() =
    static member empty = tag "dir" : HtmlTag<IDirElement>

    /// Specifies a classname for an element
    static member ``class`` = set<IDirElement> "class"

    /// Specifies the text direction  for the content in an element
    static member dir (x : DirType) = set<IDirElement> "dir" x.Value

    /// Specifies a unique id for an element
    static member id = set<IDirElement> "id"

    /// Specifies a language code for the content in an element
    static member lang = set<IDirElement> "lang"

    /// Specifies an inline style for an element
    static member style = set<IDirElement> "style"

    /// Specifies extra information about an element
    static member title = set<IDirElement> "title"



type IDivElement = inherit IClosedElement

type Div() =
    static member empty = tag "div" : HtmlTag<IDivElement>

    static member appendSetUp f (x : HtmlTag<IDivElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLDivElement> el)) x

    static member getById (x : HtmlTag<IDivElement>) =
        unbox<HTMLDivElement> (Globals.document.getElementById x.Id)



type IDlElement = inherit IClosedElement

type Dl() =
    static member empty = tag "dl" : HtmlTag<IDlElement>



type IDtElement = inherit IClosedElement

type Dt() =
    static member empty = tag "dt" : HtmlTag<IDtElement>

    static member appendSetUp f (x : HtmlTag<IDtElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLDTElement> el)) x

    static member getById (x : HtmlTag<IDtElement>) =
        unbox<HTMLDTElement> (Globals.document.getElementById x.Id)



type IEmElement = inherit IClosedElement

type Em() =
    static member empty = tag "em" : HtmlTag<IEmElement>



type IEmbedElement = inherit IUnclosedElement

type Embed() =
    static member empty = unclosedTag "embed" : HtmlTag<IEmbedElement>

    static member appendSetUp f (x : HtmlTag<IEmbedElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLEmbedElement> el)) x

    static member getById (x : HtmlTag<IEmbedElement>) =
        unbox<HTMLEmbedElement> (Globals.document.getElementById x.Id)

    /// Specifies the height of the embedded content
    static member height (x : int) = set<IEmbedElement> "height" (x.ToString())

    ///   Specifies the address of the external file to embed
    static member src = set<IEmbedElement> "src"

    /// Specifies the MIME type of the embedded content
    static member ``type`` = set<IEmbedElement> "type"

    /// Specifies the width of the embedded content
    static member width (x : int) = set<IEmbedElement> "width" (x.ToString())



type IFieldsetElement = inherit IClosedElement

type Fieldset() =
    static member empty = tag "fieldset" : HtmlTag<IFieldsetElement>

    static member appendSetUp f (x : HtmlTag<IFieldsetElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLFieldSetElement> el)) x

    static member getById (x : HtmlTag<IFieldsetElement>) =
        unbox<HTMLFieldSetElement> (Globals.document.getElementById x.Id)

    /// Specifies that a group of related form elements should be disabled
    static member disabled = setEmpty<IFieldsetElement> "disabled"

    /// Specifies one or more forms the fieldset belongs to
    static member form = set<IFieldsetElement> "form"

    /// Specifies a name for the fieldset
    static member name = set<IFieldsetElement> "name"



type IFigcaptionElement = inherit IClosedElement

type Figcaption() =
    static member empty = tag "figcaption" : HtmlTag<IFigcaptionElement>



type IFigureElement = inherit IClosedElement

type Figure() =
    static member empty = tag "figure" : HtmlTag<IFigureElement>



type IFontElement = inherit IClosedElement

type Font() =
    static member empty = tag "font" : HtmlTag<IFontElement>

    static member appendSetUp f (x : HtmlTag<IFontElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLFontElement> el)) x

    static member getById (x : HtmlTag<IFontElement>) =
        unbox<HTMLFontElement> (Globals.document.getElementById x.Id)

    /// Specifies a classname for an element
    static member ``class`` = set<IFontElement> "class"

    /// Specifies the text direction  for the content in an element
    static member dir (x : DirType) = set<IFontElement> "dir" x.Value

    /// Specifies a unique id for an element
    static member id = set<IFontElement> "id"

    /// Specifies a language code for the content in an element
    static member lang = set<IFontElement> "lang"

    /// Specifies an inline style for an element
    static member style = set<IFontElement> "style"

    /// Specifies extra information about an element
    static member title = set<IFontElement> "title"



type IFooterElement = inherit IClosedElement

type Footer() =
    static member empty = tag "footer" : HtmlTag<IFooterElement>



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

type IFormElement = inherit IClosedElement

type Form() =
    static member empty = tag "form" : HtmlTag<IFormElement>

    static member appendSetUp f (x : HtmlTag<IFormElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLFormElement> el)) x

    static member getById (x : HtmlTag<IFormElement>) =
        unbox<HTMLFormElement> (Globals.document.getElementById x.Id)

    /// Specifies the character encodings that are to be used for the form  submission
    static member accept_charset = set<IFormElement> "accept-charset"

    /// Specifies where to send the form-data when a form is submitted
    static member action = set<IFormElement> "action"

    /// Specifies whether a form should have autocomplete on or off
    static member autocomplete (x : Autocomplete) = set<IFormElement> "autocomplete" x.Value

    /// Specifies how the form-data should be encoded when submitting it to the  server (only for method="post")
    static member enctype (x : Formenctype) = set<IFormElement> "enctype" x.Value

    /// Specifies the HTTP method to use when sending form-data
    static member ``method`` (x : Formmethod) = set<IFormElement> "method" x.Value

    /// Specifies the name of a form
    static member name = set<IFormElement> "name"

    /// Specifies that the form should not be validated when submitted
    static member novalidate = setEmpty<IFormElement> "novalidate"

    /// Specifies where to display the response that is received after submitting the form
    static member target (x : IFormElementTarget) = set<IFormElement> "target" x.Value



type IFrameElement = inherit IUnclosedElement

type Frame() =
    static member empty = unclosedTag "frame" : HtmlTag<IFrameElement>

    static member appendSetUp f (x : HtmlTag<IFrameElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLFrameElement> el)) x

    static member getById (x : HtmlTag<IFrameElement>) =
        unbox<HTMLFrameElement> (Globals.document.getElementById x.Id)

    /// Specifies a classname for an element
    static member ``class`` = set<IFrameElement> "class"

    /// Specifies a unique id for an element
    static member id = set<IFrameElement> "id"

    /// Specifies an inline style for an element
    static member style = set<IFrameElement> "style"

    /// Specifies extra information about an element
    static member title = set<IFrameElement> "title"



type IFramesetElement = inherit IClosedElement

type Frameset() =
    static member empty = tag "frameset" : HtmlTag<IFramesetElement>

    static member appendSetUp f (x : HtmlTag<IFramesetElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLFrameSetElement> el)) x

    static member getById (x : HtmlTag<IFramesetElement>) =
        unbox<HTMLFrameSetElement> (Globals.document.getElementById x.Id)

    /// Specifies a classname for an element
    static member ``class`` = set<IFramesetElement> "class"

    /// Specifies a unique id for an element
    static member id = set<IFramesetElement> "id"

    /// Specifies an inline style for an element
    static member style = set<IFramesetElement> "style"

    /// Specifies extra information about an element
    static member title = set<IFramesetElement> "title"



type IHeadElement = inherit IClosedElement

type Head() =
    static member empty = tag "head" : HtmlTag<IHeadElement>

    static member appendSetUp f (x : HtmlTag<IHeadElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLHeadElement> el)) x

    static member getById (x : HtmlTag<IHeadElement>) =
        unbox<HTMLHeadElement> (Globals.document.getElementById x.Id)



type IHeaderElement = inherit IClosedElement

type Header() =
    static member empty = tag "header" : HtmlTag<IHeaderElement>



type IHrElement = inherit IUnclosedElement

type Hr() =
    static member empty = unclosedTag "hr" : HtmlTag<IHrElement>

    static member appendSetUp f (x : HtmlTag<IHrElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLHRElement> el)) x

    static member getById (x : HtmlTag<IHrElement>) =
        unbox<HTMLHRElement> (Globals.document.getElementById x.Id)



type IHtmlElement = inherit IClosedElement

type Html() =
    static member empty = tag "html" : HtmlTag<IHtmlElement>

    static member appendSetUp f (x : HtmlTag<IHtmlElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLHtmlElement> el)) x

    static member getById (x : HtmlTag<IHtmlElement>) =
        unbox<HTMLHtmlElement> (Globals.document.getElementById x.Id)

    /// Specifies the address of the document's cache manifest (for offline browsing)
    static member manifest = set<IHtmlElement> "manifest"



type IIElement = inherit IClosedElement

type I() =
    static member empty = tag "i" : HtmlTag<IIElement>



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

type IIframeElement = inherit IClosedElement

type Iframe() =
    static member empty = tag "iframe" : HtmlTag<IIframeElement>

    static member appendSetUp f (x : HtmlTag<IIframeElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLIFrameElement> el)) x

    static member getById (x : HtmlTag<IIframeElement>) =
        unbox<HTMLIFrameElement> (Globals.document.getElementById x.Id)

    /// Specifies the height of an <iframe>
    static member height (x : int) = set<IIframeElement> "height" (x.ToString())

    /// Specifies the name of an <iframe>
    static member name = set<IIframeElement> "name"

    /// Enables a set of extra restrictions for the content in the <iframe>
    static member sandbox (x : Sandbox) = set<IIframeElement> "sandbox" x.Value

    /// Specifies that the <iframe> should look like it is a part of the containing document
    static member seamless = setEmpty<IIframeElement> "seamless"

    /// Specifies the address of the document to embed in the <iframe>
    static member src = set<IIframeElement> "src"

    /// Specifies the HTML content of the page to show in the <iframe>
    static member srcdoc = set<IIframeElement> "srcdoc"

    /// Specifies the width of an <iframe>
    static member width (x : int) = set<IIframeElement> "width" (x.ToString())



type Crossorigin =
    | Anonymous
    | Use_credentials
    member x.Value =
        match x with
        | Anonymous -> "anonymous"
        | Use_credentials -> "use-credentials"

type IImgElement = inherit IUnclosedElement

type Img() =
    static member empty = unclosedTag "img" : HtmlTag<IImgElement>

    /// Specifies an alternate text for an image
    static member alt = set<IImgElement> "alt"

    /// Allow images from third-party sites that allow cross-origin access to be  used with canvas
    static member crossorigin (x : Crossorigin) = set<IImgElement> "crossorigin" x.Value

    /// Specifies the height of an image
    static member height (x : int) = set<IImgElement> "height" (x.ToString())

    /// Specifies an image as a server-side image-map
    static member ismap = setEmpty<IImgElement> "ismap"

    /// Specifies the URL of an image
    static member src = set<IImgElement> "src"

    /// Specifies an image as a client-side image-map
    static member usemap = set<IImgElement> "usemap"

    /// Specifies the width of an image
    static member width (x : int) = set<IImgElement> "width" (x.ToString())



type Accept =
    | Audio_WildCard
    | Video_WildCard
    | Image_WildCard
    | MIME_type of string
    member x.Value =
        match x with
        | Audio_WildCard -> "audio/*"
        | Video_WildCard -> "video/*"
        | Image_WildCard -> "image/*"
        | MIME_type mIME_type -> mIME_type

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
    | TextCase
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
        | TextCase -> "text"
        | Time -> "time"
        | Url -> "url"
        | Week -> "week"

type IInputElement = inherit IUnclosedElement

type Input() =
    static member empty = unclosedTag "input" : HtmlTag<IInputElement>

    static member appendSetUp f (x : HtmlTag<IInputElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLInputElement> el)) x

    static member getById (x : HtmlTag<IInputElement>) =
        unbox<HTMLInputElement> (Globals.document.getElementById x.Id)

    /// Specifies the types of files that the server accepts  (only for type="file")
    static member accept (x : Accept) = set<IInputElement> "accept" x.Value

    /// Specifies an alternate text for images (only for type="image")
    static member alt = set<IInputElement> "alt"

    /// Specifies whether an <input> element should have autocomplete  enabled
    static member autocomplete (x : Autocomplete) = set<IInputElement> "autocomplete" x.Value

    /// Specifies that an <input> element should automatically get focus when the page  loads
    static member autofocus = setEmpty<IInputElement> "autofocus"

    /// Specifies that an <input> element should be pre-selected when the page  loads (for type="checkbox" or type="radio")
    static member ``checked`` = setEmpty<IInputElement> "checked"

    /// Specifies that an <input> element should be disabled
    static member disabled = setEmpty<IInputElement> "disabled"

    /// Specifies one or more forms the <input> element belongs to
    static member form = set<IInputElement> "form"

    /// Specifies the URL of the file that will process the input control when  the form is submitted (for type="submit" and type="image")
    static member formaction = set<IInputElement> "formaction"

    /// Specifies how the form-data should be encoded when submitting it to the  server (for type="submit" and type="image")
    static member formenctype (x : Formenctype) = set<IInputElement> "formenctype" x.Value

    /// Defines the HTTP  method for sending data to the action URL (for type="submit" and type="image")
    static member formmethod (x : Formmethod) = set<IInputElement> "formmethod" x.Value

    /// Defines that form elements should not be validated when submitted
    static member formnovalidate = setEmpty<IInputElement> "formnovalidate"

    /// Specifies where to display the response that is received after submitting  the form (for type="submit" and type="image")
    static member formtarget (x : Target) = set<IInputElement> "formtarget" x.Value

    /// Specifies the height of an <input> element (only for type="image")
    static member height (x : int) = set<IInputElement> "height" (x.ToString())

    /// Refers to a <datalist> element that contains pre-defined options for an  <input> element
    static member list = set<IInputElement> "list"

    /// Specifies the maximum value for an <input> element
    static member max (x : Max) = set<IInputElement> "max" x.Value

    /// Specifies the maximum number of characters allowed in an <input> element
    static member maxlength = set<IInputElement> "maxlength"

    /// Specifies a minimum value for an <input> element
    static member min (x : Max) = set<IInputElement> "min" x.Value

    /// Specifies that a user can enter more than one value in an <input>  element
    static member multiple = setEmpty<IInputElement> "multiple"

    /// Specifies the name of an <input> element
    static member name = set<IInputElement> "name"

    /// Specifies a regular expression that an <input> element's value is  checked against
    static member pattern = set<IInputElement> "pattern"

    /// Specifies a short hint that describes the expected value of an <input>  element
    static member placeholder = set<IInputElement> "placeholder"

    /// Specifies that an input field is read-only
    static member ``readonly`` = setEmpty<IInputElement> "readonly"

    /// Specifies that an input field must be filled out before submitting the  form
    static member required = setEmpty<IInputElement> "required"

    /// Specifies the width, in characters, of an <input> element
    static member size = set<IInputElement> "size"

    /// Specifies the URL of the image to use as a submit button (only for  type="image")
    static member src = set<IInputElement> "src"

    /// Specifies the legal number intervals for an input field
    static member step = set<IInputElement> "step"

    /// Specifies the type <input> element to display
    static member ``type`` (x : IInputElementType) = set<IInputElement> "type" x.Value

    /// Specifies the value of an <input> element   
    static member value = set<IInputElement> "value"

    /// Specifies the width of an <input> element (only for type="image")
    static member width (x : int) = set<IInputElement> "width" (x.ToString())



type IInsElement = inherit IClosedElement

type Ins() =
    static member empty = tag "ins" : HtmlTag<IInsElement>

    /// Specifies a URL to a document that explains the reason why the text was  inserted/changed
    static member cite = set<IInsElement> "cite"

    /// Specifies the date and time when the text was inserted/changed
    static member datetime = set<IInsElement> "datetime"



type IKbdElement = inherit IClosedElement

type Kbd() =
    static member empty = tag "kbd" : HtmlTag<IKbdElement>



type Keytype =
    | Rsa
    | Dsa
    | Ec
    member x.Value =
        match x with
        | Rsa -> "rsa"
        | Dsa -> "dsa"
        | Ec -> "ec"

type IKeygenElement = inherit IUnclosedElement

type Keygen() =
    static member empty = unclosedTag "keygen" : HtmlTag<IKeygenElement>

    /// Specifies that a <keygen> element should automatically get focus when the page loads
    static member autofocus = setEmpty<IKeygenElement> "autofocus"

    /// Specifies that the value of the <keygen> element should be challenged when submitted
    static member challenge = setEmpty<IKeygenElement> "challenge"

    /// Specifies that a <keygen> element should be disabled
    static member disabled = setEmpty<IKeygenElement> "disabled"

    /// Specifies one or more forms the <keygen> element belongs to
    static member form = set<IKeygenElement> "form"

    /// Specifies the security algorithm of the key
    static member keytype (x : Keytype) = set<IKeygenElement> "keytype" x.Value

    /// Defines a name for the <keygen> element
    static member name = set<IKeygenElement> "name"



type ILabelElement = inherit IClosedElement

type Label() =
    static member empty = tag "label" : HtmlTag<ILabelElement>

    static member appendSetUp f (x : HtmlTag<ILabelElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLLabelElement> el)) x

    static member getById (x : HtmlTag<ILabelElement>) =
        unbox<HTMLLabelElement> (Globals.document.getElementById x.Id)

    /// Specifies which form element a label is bound to
    static member ``for`` = set<ILabelElement> "for"

    /// Specifies one or more forms the label belongs to
    static member form = set<ILabelElement> "form"



type ILegendElement = inherit IClosedElement

type Legend() =
    static member empty = tag "legend" : HtmlTag<ILegendElement>

    static member appendSetUp f (x : HtmlTag<ILegendElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLLegendElement> el)) x

    static member getById (x : HtmlTag<ILegendElement>) =
        unbox<HTMLLegendElement> (Globals.document.getElementById x.Id)



type ILiElement = inherit IClosedElement

type Li() =
    static member empty = tag "li" : HtmlTag<ILiElement>

    static member appendSetUp f (x : HtmlTag<ILiElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLLIElement> el)) x

    static member getById (x : HtmlTag<ILiElement>) =
        unbox<HTMLLIElement> (Globals.document.getElementById x.Id)

    /// Specifies the value of a list item. The following list items will increment  from that number (only for <ol> lists)
    static member value = set<ILiElement> "value"



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
    | TagCase
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
        | TagCase -> "tag"
        | Up -> "up"

type Sizes =
    | HeightxWidth of string
    | Any
    member x.Value =
        match x with
        | HeightxWidth heightxWidth -> heightxWidth
        | Any -> "any"

type ILinkElement = inherit IUnclosedElement

type Link() =
    static member empty = unclosedTag "link" : HtmlTag<ILinkElement>

    static member appendSetUp f (x : HtmlTag<ILinkElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLLinkElement> el)) x

    static member getById (x : HtmlTag<ILinkElement>) =
        unbox<HTMLLinkElement> (Globals.document.getElementById x.Id)

    /// Specifies the location of the linked document
    static member href = set<ILinkElement> "href"

    /// Specifies the language of the text in the linked document
    static member hreflang = set<ILinkElement> "hreflang"

    /// Specifies on what device the linked document will be displayed
    static member media = set<ILinkElement> "media"

    /// Required. Specifies the relationship between the current document and the linked  document
    static member rel (x : ILinkElementRel) = set<ILinkElement> "rel" x.Value

    /// Specifies the size of the linked resource. Only for rel="icon"
    static member sizes (x : Sizes) = set<ILinkElement> "sizes" x.Value

    /// Specifies the MIME type of the linked document
    static member ``type`` = set<ILinkElement> "type"



type IMapElementElement = inherit IClosedElement

type MapElement() =
    static member empty = tag "map" : HtmlTag<IMapElementElement>

    static member appendSetUp f (x : HtmlTag<IMapElementElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLMapElement> el)) x

    static member getById (x : HtmlTag<IMapElementElement>) =
        unbox<HTMLMapElement> (Globals.document.getElementById x.Id)

    /// Required. Specifies the name of an image-map
    static member name = set<IMapElementElement> "name"



type IMarkElement = inherit IClosedElement

type Mark() =
    static member empty = tag "mark" : HtmlTag<IMarkElement>



type IMenuElementType =
    | Context
    | Toolbar
    | List
    member x.Value =
        match x with
        | Context -> "context"
        | Toolbar -> "toolbar"
        | List -> "list"

type IMenuElement = inherit IClosedElement

type Menu() =
    static member empty = tag "menu" : HtmlTag<IMenuElement>

    static member appendSetUp f (x : HtmlTag<IMenuElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLMenuElement> el)) x

    static member getById (x : HtmlTag<IMenuElement>) =
        unbox<HTMLMenuElement> (Globals.document.getElementById x.Id)

    /// Specifies a visible label for the menu
    static member label = set<IMenuElement> "label"

    /// Specifies which type of menu to display. Default value is "list"
    static member ``type`` (x : IMenuElementType) = set<IMenuElement> "type" x.Value



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

type IMetaElement = inherit IUnclosedElement

type Meta() =
    static member empty = unclosedTag "meta" : HtmlTag<IMetaElement>

    static member appendSetUp f (x : HtmlTag<IMetaElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLMetaElement> el)) x

    static member getById (x : HtmlTag<IMetaElement>) =
        unbox<HTMLMetaElement> (Globals.document.getElementById x.Id)

    /// Specifies the character encoding for the HTML document
    static member charset = set<IMetaElement> "charset"

    /// Gives the value associated with the http-equiv or name attribute
    static member content = set<IMetaElement> "content"

    /// Provides an HTTP header for the information/value of the content  attribute
    static member http_equiv (x : Http_equiv) = set<IMetaElement> "http-equiv" x.Value

    /// Specifies a name for the metadata
    static member name (x : Name) = set<IMetaElement> "name" x.Value



type IMeterElement = inherit IClosedElement

type Meter() =
    static member empty = tag "meter" : HtmlTag<IMeterElement>

    /// Specifies one or more forms the <meter> element belongs to
    static member form = set<IMeterElement> "form"

    /// Specifies the range that is considered to be a high value
    static member high = set<IMeterElement> "high"

    /// Specifies the range that is considered to be a low value
    static member low = set<IMeterElement> "low"

    /// Specifies the maximum value of the range
    static member max = set<IMeterElement> "max"

    /// Specifies the minimum value of the range
    static member min = set<IMeterElement> "min"

    /// Specifies what value is the optimal value for the gauge
    static member optimum = set<IMeterElement> "optimum"

    /// Required. Specifies the current value of the gauge
    static member value = set<IMeterElement> "value"



type INavElement = inherit IClosedElement

type Nav() =
    static member empty = tag "nav" : HtmlTag<INavElement>



type INoframesElement = inherit IClosedElement

type Noframes() =
    static member empty = tag "noframes" : HtmlTag<INoframesElement>

    /// Specifies a classname for an element
    static member ``class`` = set<INoframesElement> "class"

    /// Specifies the text direction  for the content in an element
    static member dir (x : DirType) = set<INoframesElement> "dir" x.Value

    /// Specifies a unique id for an element
    static member id = set<INoframesElement> "id"

    /// Specifies a language code for the content in an element
    static member lang = set<INoframesElement> "lang"

    /// Specifies an inline style for an element
    static member style = set<INoframesElement> "style"

    /// Specifies extra information about an element
    static member title = set<INoframesElement> "title"

    /// Specifies a language code for the content in an element, in  XHTML documents
    static member xml_lang = set<INoframesElement> "xml:lang"



type INoscriptElement = inherit IClosedElement

type Noscript() =
    static member empty = tag "noscript" : HtmlTag<INoscriptElement>



type IObjectElement = inherit IClosedElement

type Object() =
    static member empty = tag "object" : HtmlTag<IObjectElement>

    static member appendSetUp f (x : HtmlTag<IObjectElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLObjectElement> el)) x

    static member getById (x : HtmlTag<IObjectElement>) =
        unbox<HTMLObjectElement> (Globals.document.getElementById x.Id)

    /// Specifies the URL of the resource to be used by the object
    static member data = set<IObjectElement> "data"

    /// Specifies one or more forms the object belongs to
    static member form = set<IObjectElement> "form"

    /// Specifies the height of the object
    static member height (x : int) = set<IObjectElement> "height" (x.ToString())

    /// Specifies a name for the object
    static member name = set<IObjectElement> "name"

    /// Specifies the MIME type of data specified in the data attribute
    static member ``type`` = set<IObjectElement> "type"

    /// Specifies the name of a client-side image map to be used with the object
    static member usemap = set<IObjectElement> "usemap"

    /// Specifies the width of the object
    static member width (x : int) = set<IObjectElement> "width" (x.ToString())



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

type IOlElement = inherit IClosedElement

type Ol() =
    static member empty = tag "ol" : HtmlTag<IOlElement>

    /// Specifies that the list order should be descending (9,8,7...)
    static member reversed = setEmpty<IOlElement> "reversed"

    /// Specifies the start value of an ordered list
    static member start = set<IOlElement> "start"

    /// Specifies the kind of marker to use in the list
    static member ``type`` (x : IOlElementType) = set<IOlElement> "type" x.Value



type IOptgroupElement = inherit IClosedElement

type Optgroup() =
    static member empty = tag "optgroup" : HtmlTag<IOptgroupElement>

    static member appendSetUp f (x : HtmlTag<IOptgroupElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLOptGroupElement> el)) x

    static member getById (x : HtmlTag<IOptgroupElement>) =
        unbox<HTMLOptGroupElement> (Globals.document.getElementById x.Id)

    /// Specifies that an option-group should be disabled
    static member disabled = setEmpty<IOptgroupElement> "disabled"

    /// Specifies a label for an option-group
    static member label = set<IOptgroupElement> "label"



type IOptionElement = inherit IClosedElement

type Option() =
    static member empty = tag "option" : HtmlTag<IOptionElement>

    static member appendSetUp f (x : HtmlTag<IOptionElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLOptionElement> el)) x

    static member getById (x : HtmlTag<IOptionElement>) =
        unbox<HTMLOptionElement> (Globals.document.getElementById x.Id)

    /// Specifies that an option should be disabled
    static member disabled = setEmpty<IOptionElement> "disabled"

    /// Specifies a shorter label for an option
    static member label = set<IOptionElement> "label"

    /// Specifies that an option should be pre-selected when the page loads
    static member selected = setEmpty<IOptionElement> "selected"

    /// Specifies the value to be sent to a server
    static member value = set<IOptionElement> "value"



type IOutputElement = inherit IClosedElement

type Output() =
    static member empty = tag "output" : HtmlTag<IOutputElement>

    /// Specifies the relationship between the result of the calculation, and the elements used in the calculation
    static member ``for`` = set<IOutputElement> "for"

    /// Specifies one or more forms the output element belongs to
    static member form = set<IOutputElement> "form"

    /// Specifies a name for the output element
    static member name = set<IOutputElement> "name"



type IPElement = inherit IClosedElement

type P() =
    static member empty = tag "p" : HtmlTag<IPElement>



type IParamElement = inherit IUnclosedElement

type Param() =
    static member empty = unclosedTag "param" : HtmlTag<IParamElement>

    static member appendSetUp f (x : HtmlTag<IParamElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLParamElement> el)) x

    static member getById (x : HtmlTag<IParamElement>) =
        unbox<HTMLParamElement> (Globals.document.getElementById x.Id)

    /// Specifies the name of a parameter
    static member name = set<IParamElement> "name"

    /// Specifies the value of the parameter
    static member value = set<IParamElement> "value"



type IPreElement = inherit IClosedElement

type Pre() =
    static member empty = tag "pre" : HtmlTag<IPreElement>

    static member appendSetUp f (x : HtmlTag<IPreElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLPreElement> el)) x

    static member getById (x : HtmlTag<IPreElement>) =
        unbox<HTMLPreElement> (Globals.document.getElementById x.Id)



type IProgressElement = inherit IClosedElement

type Progress() =
    static member empty = tag "progress" : HtmlTag<IProgressElement>

    static member appendSetUp f (x : HtmlTag<IProgressElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLProgressElement> el)) x

    static member getById (x : HtmlTag<IProgressElement>) =
        unbox<HTMLProgressElement> (Globals.document.getElementById x.Id)

    /// Specifies how much work the task requires in total
    static member max = set<IProgressElement> "max"

    /// Specifies how much of the task has been completed
    static member value = set<IProgressElement> "value"



type IQElement = inherit IClosedElement

type Q() =
    static member empty = tag "q" : HtmlTag<IQElement>

    /// Specifies the source URL of the quote
    static member cite = set<IQElement> "cite"



type IRpElement = inherit IClosedElement

type Rp() =
    static member empty = tag "rp" : HtmlTag<IRpElement>



type IRtElement = inherit IClosedElement

type Rt() =
    static member empty = tag "rt" : HtmlTag<IRtElement>



type IRubyElement = inherit IClosedElement

type Ruby() =
    static member empty = tag "ruby" : HtmlTag<IRubyElement>



type ISElement = inherit IClosedElement

type S() =
    static member empty = tag "s" : HtmlTag<ISElement>



type ISampElement = inherit IClosedElement

type Samp() =
    static member empty = tag "samp" : HtmlTag<ISampElement>



type IScriptElement = inherit IClosedElement

type Script() =
    static member empty = tag "script" : HtmlTag<IScriptElement>

    static member appendSetUp f (x : HtmlTag<IScriptElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLScriptElement> el)) x

    static member getById (x : HtmlTag<IScriptElement>) =
        unbox<HTMLScriptElement> (Globals.document.getElementById x.Id)

    /// Specifies that the script is executed asynchronously (only for external scripts)
    static member ``async`` = setEmpty<IScriptElement> "async"

    /// Specifies the character encoding used in an external script  file
    static member charset = set<IScriptElement> "charset"

    /// Specifies that the script is executed when the page has finished parsing  (only for external scripts)
    static member defer = setEmpty<IScriptElement> "defer"

    /// Specifies the URL of an external script file
    static member src = set<IScriptElement> "src"

    /// Specifies the MIME type of the script
    static member ``type`` = set<IScriptElement> "type"



type ISectionElement = inherit IClosedElement

type Section() =
    static member empty = tag "section" : HtmlTag<ISectionElement>



type ISelectElement = inherit IClosedElement

type Select() =
    static member empty = tag "select" : HtmlTag<ISelectElement>

    static member appendSetUp f (x : HtmlTag<ISelectElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLSelectElement> el)) x

    static member getById (x : HtmlTag<ISelectElement>) =
        unbox<HTMLSelectElement> (Globals.document.getElementById x.Id)

    /// Specifies that the drop-down list should automatically get focus when  the page loads
    static member autofocus = setEmpty<ISelectElement> "autofocus"

    /// Specifies that a drop-down list should be disabled
    static member disabled = setEmpty<ISelectElement> "disabled"

    /// Defines one or more forms the select field belongs to
    static member form = set<ISelectElement> "form"

    /// Specifies that multiple options can be selected at once
    static member multiple = setEmpty<ISelectElement> "multiple"

    /// Defines a name for the drop-down list
    static member name = set<ISelectElement> "name"

    /// Specifies that the user is required to select a value before submitting  the form
    static member required = setEmpty<ISelectElement> "required"

    /// Defines the number of visible options in a drop-down list
    static member size = set<ISelectElement> "size"



type ISmallElement = inherit IClosedElement

type Small() =
    static member empty = tag "small" : HtmlTag<ISmallElement>



type ISourceElement = inherit IUnclosedElement

type Source() =
    static member empty = unclosedTag "source" : HtmlTag<ISourceElement>

    static member appendSetUp f (x : HtmlTag<ISourceElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLSourceElement> el)) x

    static member getById (x : HtmlTag<ISourceElement>) =
        unbox<HTMLSourceElement> (Globals.document.getElementById x.Id)

    /// Specifies the type of media resource
    static member media = set<ISourceElement> "media"

    /// Specifies the URL of the media file
    static member src = set<ISourceElement> "src"

    /// Specifies the MIME type of the media resource
    static member ``type`` = set<ISourceElement> "type"



type ISpanElement = inherit IClosedElement

type Span() =
    static member empty = tag "span" : HtmlTag<ISpanElement>

    static member appendSetUp f (x : HtmlTag<ISpanElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLSpanElement> el)) x

    static member getById (x : HtmlTag<ISpanElement>) =
        unbox<HTMLSpanElement> (Globals.document.getElementById x.Id)



type IStrikeElement = inherit IClosedElement

type Strike() =
    static member empty = tag "strike" : HtmlTag<IStrikeElement>

    /// Specifies a classname for an element
    static member ``class`` = set<IStrikeElement> "class"

    /// Specifies the text direction  for the content in an element
    static member dir (x : DirType) = set<IStrikeElement> "dir" x.Value

    /// Specifies a unique id for an element
    static member id = set<IStrikeElement> "id"

    /// Specifies a language code for the content in an element
    static member lang = set<IStrikeElement> "lang"

    /// Specifies an inline style for an element
    static member style = set<IStrikeElement> "style"

    /// Specifies extra information about an element
    static member title = set<IStrikeElement> "title"



type IStrongElement = inherit IClosedElement

type Strong() =
    static member empty = tag "strong" : HtmlTag<IStrongElement>



type IStyleElementType =
    | Text_css
    member x.Value =
        match x with
        | Text_css -> "text/css"

type IStyleElement = inherit IClosedElement

type Style() =
    static member empty = tag "style" : HtmlTag<IStyleElement>

    static member appendSetUp f (x : HtmlTag<IStyleElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLStyleElement> el)) x

    static member getById (x : HtmlTag<IStyleElement>) =
        unbox<HTMLStyleElement> (Globals.document.getElementById x.Id)

    /// Specifies what media/device the media resource is optimized for
    static member media = set<IStyleElement> "media"

    /// Specifies that the styles only apply to this element's parent element  and that element's child elements
    static member scoped = setEmpty<IStyleElement> "scoped"

    /// Specifies the MIME type of the style sheet
    static member ``type`` (x : IStyleElementType) = set<IStyleElement> "type" x.Value



type ISubElement = inherit IClosedElement

type Sub() =
    static member empty = tag "sub" : HtmlTag<ISubElement>



type ISummaryElement = inherit IClosedElement

type Summary() =
    static member empty = tag "summary" : HtmlTag<ISummaryElement>



type ISupElement = inherit IClosedElement

type Sup() =
    static member empty = tag "sup" : HtmlTag<ISupElement>



type Border =
    | Number of float
    | Empty
    member x.Value =
        match x with
        | Number number -> number.ToString()
        | Empty -> ""

type ITableElement = inherit IClosedElement

type Table() =
    static member empty = tag "table" : HtmlTag<ITableElement>

    static member appendSetUp f (x : HtmlTag<ITableElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLTableElement> el)) x

    static member getById (x : HtmlTag<ITableElement>) =
        unbox<HTMLTableElement> (Globals.document.getElementById x.Id)

    /// Specifies whether the table cells should have borders or not
    static member border (x : Border) = set<ITableElement> "border" x.Value



type ITbodyElement = inherit IClosedElement

type Tbody() =
    static member empty = tag "tbody" : HtmlTag<ITbodyElement>



type ITdElement = inherit IClosedElement

type Td() =
    static member empty = tag "td" : HtmlTag<ITdElement>

    /// Specifies the number of columns a cell should span
    static member colspan = set<ITdElement> "colspan"

    /// Specifies one or more header cells a cell is related to
    static member headers = set<ITdElement> "headers"

    /// Sets the number of rows a cell should span
    static member rowspan = set<ITdElement> "rowspan"



type Wrap =
    | Hard
    | Soft
    member x.Value =
        match x with
        | Hard -> "hard"
        | Soft -> "soft"

type ITextareaElement = inherit IUnclosedElement

type Textarea() =
    static member empty = unclosedTag "textarea" : HtmlTag<ITextareaElement>

    static member appendSetUp f (x : HtmlTag<ITextareaElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLTextAreaElement> el)) x

    static member getById (x : HtmlTag<ITextareaElement>) =
        unbox<HTMLTextAreaElement> (Globals.document.getElementById x.Id)

    /// Specifies that a text area should automatically get focus when the page  loads
    static member autofocus = setEmpty<ITextareaElement> "autofocus"

    /// Specifies the visible width of a text area
    static member cols = set<ITextareaElement> "cols"

    /// Specifies that a text area should be disabled
    static member disabled = setEmpty<ITextareaElement> "disabled"

    /// Specifies one or more forms the text area belongs to
    static member form = set<ITextareaElement> "form"

    /// Specifies the maximum number of characters allowed in the text area
    static member maxlength = set<ITextareaElement> "maxlength"

    /// Specifies a name for a text area
    static member name = set<ITextareaElement> "name"

    /// Specifies a short hint that describes the expected value of a text area
    static member placeholder = set<ITextareaElement> "placeholder"

    /// Specifies that a text area should be read-only
    static member ``readonly`` = setEmpty<ITextareaElement> "readonly"

    /// Specifies that a text area is required/must be filled out
    static member required = setEmpty<ITextareaElement> "required"

    /// Specifies the visible number of lines in a text area
    static member rows = set<ITextareaElement> "rows"

    /// Specifies how the text in a text area is to be wrapped when submitted in a form
    static member wrap (x : Wrap) = set<ITextareaElement> "wrap" x.Value



type ITfootElement = inherit IClosedElement

type Tfoot() =
    static member empty = tag "tfoot" : HtmlTag<ITfootElement>



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

type IThElement = inherit IClosedElement

type Th() =
    static member empty = tag "th" : HtmlTag<IThElement>

    /// Specifies the number of columns a header cell should span
    static member colspan = set<IThElement> "colspan"

    /// Specifies one or more header cells a cell is related to
    static member headers = set<IThElement> "headers"

    /// Specifies the number of rows a header cell should span
    static member rowspan = set<IThElement> "rowspan"

    /// Specifies whether a header cell is a header for a column, row, or group  of columns or rows
    static member scope (x : Scope) = set<IThElement> "scope" x.Value



type ITheadElement = inherit IClosedElement

type Thead() =
    static member empty = tag "thead" : HtmlTag<ITheadElement>



type ITimeElement = inherit IClosedElement

type Time() =
    static member empty = tag "time" : HtmlTag<ITimeElement>

    /// Gives the date/time being specified. Otherwise, the date/time is given  by the element's contents
    static member datetime = set<ITimeElement> "datetime"



type ITitleElement = inherit IClosedElement

type Title() =
    static member empty = tag "title" : HtmlTag<ITitleElement>

    static member appendSetUp f (x : HtmlTag<ITitleElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLTitleElement> el)) x

    static member getById (x : HtmlTag<ITitleElement>) =
        unbox<HTMLTitleElement> (Globals.document.getElementById x.Id)



type ITrElement = inherit IClosedElement

type Tr() =
    static member empty = tag "tr" : HtmlTag<ITrElement>



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

type ITrackElement = inherit IUnclosedElement

type Track() =
    static member empty = unclosedTag "track" : HtmlTag<ITrackElement>

    static member appendSetUp f (x : HtmlTag<ITrackElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLTrackElement> el)) x

    static member getById (x : HtmlTag<ITrackElement>) =
        unbox<HTMLTrackElement> (Globals.document.getElementById x.Id)

    /// Specifies that the track is to be enabled if the user's preferences do  not indicate that another track would be more appropriate
    static member ``default`` = setEmpty<ITrackElement> "default"

    /// Specifies the kind of text track
    static member kind (x : Kind) = set<ITrackElement> "kind" x.Value

    /// Specifies the title of the text track
    static member label = set<ITrackElement> "label"

    /// Required. Specifies the URL of the track file
    static member src = set<ITrackElement> "src"

    /// Specifies the language of the track text data (required if kind="subtitles")
    static member srclang = set<ITrackElement> "srclang"



type ITtElement = inherit IClosedElement

type Tt() =
    static member empty = tag "tt" : HtmlTag<ITtElement>

    /// Specifies a classname for an element
    static member ``class`` = set<ITtElement> "class"

    /// Specifies the text direction for the content in an element
    static member dir (x : DirType) = set<ITtElement> "dir" x.Value

    /// Specifies a unique id for an element
    static member id = set<ITtElement> "id"

    /// Specifies a language code for the content in an element
    static member lang = set<ITtElement> "lang"

    /// Specifies an inline style for an element
    static member style = set<ITtElement> "style"

    /// Specifies extra information about an element
    static member title = set<ITtElement> "title"

    /// Specifies a language code for the content in an element, in  XHTML documents
    static member xml_lang = set<ITtElement> "xml:lang"



type IUElement = inherit IClosedElement

type U() =
    static member empty = tag "u" : HtmlTag<IUElement>



type IUlElement = inherit IClosedElement

type Ul() =
    static member empty = tag "ul" : HtmlTag<IUlElement>



type IVarElement = inherit IClosedElement

type Var() =
    static member empty = tag "var" : HtmlTag<IVarElement>



type IVideoElement = inherit IClosedElement

type Video() =
    static member empty = tag "video" : HtmlTag<IVideoElement>

    static member appendSetUp f (x : HtmlTag<IVideoElement>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLVideoElement> el)) x

    static member getById (x : HtmlTag<IVideoElement>) =
        unbox<HTMLVideoElement> (Globals.document.getElementById x.Id)

    /// Specifies that the video will start playing as soon as it is ready
    static member autoplay = setEmpty<IVideoElement> "autoplay"

    /// Specifies that video controls should be displayed (such as a play/pause button etc).
    static member controls = setEmpty<IVideoElement> "controls"

    /// Sets the height of the video player
    static member height (x : int) = set<IVideoElement> "height" (x.ToString())

    /// Specifies that the video will start over again, every time it is finished
    static member loop = setEmpty<IVideoElement> "loop"

    /// Specifies that the audio output of the video should be muted
    static member muted = setEmpty<IVideoElement> "muted"

    /// Specifies an image to be shown while the video is downloading, or until the user hits the play button
    static member poster = set<IVideoElement> "poster"

    /// Specifies if and how the author thinks the video should be loaded when the page loads
    static member preload (x : Preload) = set<IVideoElement> "preload" x.Value

    /// Specifies the URL of the video file
    static member src = set<IVideoElement> "src"

    /// Sets the width of the video player
    static member width (x : int) = set<IVideoElement> "width" (x.ToString())



type IWbrElement = inherit IUnclosedElement

type Wbr() =
    static member empty = unclosedTag "wbr" : HtmlTag<IWbrElement>