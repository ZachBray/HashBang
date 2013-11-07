[<AutoOpen; ReflectedDefinition>]
module TypeInferred.HashBang.Html.Tags
#if INTERACTIVE
#r @"..\packages\FunScript.1.1.0.14\lib\net40\FunScript.dll"
#r @"..\packages\FunScript.1.1.0.14\lib\net40\FunScript.Interop.dll"
#r @"..\packages\FunScript.TypeScript.Binding.lib.1.1.0.13\lib\net40\FunScript.TypeScript.Binding.lib.dll"
#r @"..\packages\FunScript.TypeScript.Binding.jquery.1.1.0.13\lib\net40\FunScript.TypeScript.Binding.jquery.dll"
#endif

open FunScript

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

module Unchecked =

    let alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    
    [<JSEmitInline("Math.floor(Math.random() * {0})")>]
    let random maxExcl =
        let rand = System.Random()
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

module A =
    type IA = inherit IClosedElement
    let empty = tag "a" : HtmlTag<IA>

    /// Specifies the hyperlink target to be downloaded
    let download = set<IA> "download"

    /// Specifies the URL of the page the link goes to
    let href = set<IA> "href"

    /// Specifies the language of the linked document
    let hreflang = set<IA> "hreflang"

    /// Specifies what media/device the linked document is optimized for
    let media = set<IA> "media"

    /// Specifies the relationship between the current document and the linked document
    let rel (x : Rel) = set<IA> "rel" x.Value

    /// Specifies where to open the linked document
    let target (x : Target) = set<IA> "target" x.Value

    /// Specifies the MIME  type of the linked document
    let ``type`` = set<IA> "type"



module Abbr =
    type IAbbr = inherit IClosedElement
    let empty = tag "abbr" : HtmlTag<IAbbr>



type DirType =
    | Rtl
    | Ltr
    member x.Value =
        match x with
        | Rtl -> "rtl"
        | Ltr -> "ltr"

module Acronym =
    type IAcronym = inherit IClosedElement
    let empty = tag "acronym" : HtmlTag<IAcronym>

    /// Specifies a classname for an element
    let ``class`` = set<IAcronym> "class"

    /// Specifies the text direction  for the content in an element
    let dir (x : DirType) = set<IAcronym> "dir" x.Value

    /// Specifies a unique id for an element
    let id = set<IAcronym> "id"

    /// Specifies a language code for the content in an element
    let lang = set<IAcronym> "lang"

    /// Specifies an inline style for an element
    let style = set<IAcronym> "style"

    /// Specifies extra information about an element
    let title = set<IAcronym> "title"

    /// Specifies a language code for the content in an element, in  XHTML documents
    let xml_lang = set<IAcronym> "xml:lang"



module Address =
    type IAddress = inherit IClosedElement
    let empty = tag "address" : HtmlTag<IAddress>



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

module Applet =
    type IApplet = inherit IClosedElement
    let empty = tag "applet" : HtmlTag<IApplet>

    let appendSetUp f (x : HtmlTag<IApplet>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLAppletElement> el)) x

    /// Specifies the file name of a Java applet
    let code = set<IApplet> "code"

    /// Specifies a reference to a serialized representation of an  applet
    let ``object`` = set<IApplet> "object"

    /// Specifies the alignment of an applet according to  surrounding elements
    let align (x : Align) = set<IApplet> "align" x.Value

    /// Specifies an alternate text for an applet
    let alt = set<IApplet> "alt"

    /// Specifies the location of an archive file
    let archive = set<IApplet> "archive"

    /// Specifies a relative base URL for applets specified in the  code attribute
    let codebase = set<IApplet> "codebase"

    /// Specifies the height of an applet
    let height (x : int) = set<IApplet> "height" (x.ToString())

    /// Defines the horizontal spacing around an applet
    let hspace (x : int) = set<IApplet> "hspace" (x.ToString())

    /// Defines the name for an applet (to use in scripts)
    let name = set<IApplet> "name"

    /// Defines the vertical spacing around an applet
    let vspace (x : int) = set<IApplet> "vspace" (x.ToString())

    /// Specifies the width of an applet
    let width (x : int) = set<IApplet> "width" (x.ToString())

    /// Specifies a classname for an element
    let ``class`` = set<IApplet> "class"

    /// Specifies a unique id for an element
    let id = set<IApplet> "id"

    /// Specifies an inline style for an element
    let style = set<IApplet> "style"

    /// Specifies extra information about an element
    let title = set<IApplet> "title"



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

module Area =
    type IArea = inherit IUnclosedElement
    let empty = unclosedTag "area" : HtmlTag<IArea>

    let appendSetUp f (x : HtmlTag<IArea>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLAreaElement> el)) x

    /// Specifies an alternate text for the area. Required if the href attribute is present
    let alt = set<IArea> "alt"

    /// Specifies the coordinates of the area
    let coords = set<IArea> "coords"

    /// Specifies the hyperlink target to be downloaded
    let download = set<IArea> "download"

    /// Specifies the hyperlink target for the area
    let href = set<IArea> "href"

    /// Specifies the language of the target URL
    let hreflang = set<IArea> "hreflang"

    /// Specifies what media/device the target URL is optimized for
    let media = set<IArea> "media"

    /// Specifies the relationship between the current document and  the target URL
    let rel (x : Rel) = set<IArea> "rel" x.Value

    /// Specifies the shape of the area
    let shape (x : Shape) = set<IArea> "shape" x.Value

    /// Specifies where to open the target URL
    let target (x : Target) = set<IArea> "target" x.Value

    /// Specifies the MIME  type of the target URL
    let ``type`` = set<IArea> "type"



module Article =
    type IArticle = inherit IClosedElement
    let empty = tag "article" : HtmlTag<IArticle>



module Aside =
    type IAside = inherit IClosedElement
    let empty = tag "aside" : HtmlTag<IAside>



type Preload =
    | Auto
    | Metadata
    | NoneCase
    member x.Value =
        match x with
        | Auto -> "auto"
        | Metadata -> "metadata"
        | NoneCase -> "none"

module Audio =
    type IAudio = inherit IClosedElement
    let empty = tag "audio" : HtmlTag<IAudio>

    let appendSetUp f (x : HtmlTag<IAudio>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLAudioElement> el)) x

    /// Specifies that the audio will start playing as soon as it is ready
    let autoplay = setEmpty<IAudio> "autoplay"

    /// Specifies that audio controls should be displayed (such as a play/pause  button etc).
    let controls = setEmpty<IAudio> "controls"

    /// Specifies that the audio will start over again, every time it  is finished
    let loop = setEmpty<IAudio> "loop"

    /// Specifies that the audio output should be muted
    let muted = setEmpty<IAudio> "muted"

    /// Specifies if and how the author thinks the audio should be loaded when the page loads
    let preload (x : Preload) = set<IAudio> "preload" x.Value

    ///   Specifies the URL of the audio file
    let src = set<IAudio> "src"



module B =
    type IB = inherit IClosedElement
    let empty = tag "b" : HtmlTag<IB>



module Base =
    type IBase = inherit IUnclosedElement
    let empty = unclosedTag "base" : HtmlTag<IBase>

    let appendSetUp f (x : HtmlTag<IBase>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLBaseElement> el)) x

    /// Specifies the base URL for all relative URLs in the page
    let href = set<IBase> "href"

    /// Specifies the default target for all hyperlinks and forms in the page
    let target (x : Target) = set<IBase> "target" x.Value



module Basefont =
    type IBasefont = inherit IUnclosedElement
    let empty = unclosedTag "basefont" : HtmlTag<IBasefont>

    let appendSetUp f (x : HtmlTag<IBasefont>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLBaseFontElement> el)) x

    /// Specifies a classname for an element
    let ``class`` = set<IBasefont> "class"

    /// Specifies the text direction  for the content in an element
    let dir (x : DirType) = set<IBasefont> "dir" x.Value

    /// Specifies a unique id for an element
    let id = set<IBasefont> "id"

    /// Specifies a language code for the content in an element
    let lang = set<IBasefont> "lang"

    /// Specifies an inline style for an element
    let style = set<IBasefont> "style"

    /// Specifies extra information about an element
    let title = set<IBasefont> "title"



module Bdi =
    type IBdi = inherit IClosedElement
    let empty = tag "bdi" : HtmlTag<IBdi>



module Bdo =
    type IBdo = inherit IClosedElement
    let empty = tag "bdo" : HtmlTag<IBdo>

    /// Required. Specifies the text direction of the text inside the <bdo> element
    let dir (x : DirType) = set<IBdo> "dir" x.Value



module Big =
    type IBig = inherit IClosedElement
    let empty = tag "big" : HtmlTag<IBig>

    /// Specifies a classname for an element
    let ``class`` = set<IBig> "class"

    /// Specifies the text direction for the content in an element
    let dir (x : DirType) = set<IBig> "dir" x.Value

    /// Specifies a unique id for an element
    let id = set<IBig> "id"

    /// Specifies a language code for the content in an element
    let lang = set<IBig> "lang"

    /// Specifies an inline style for an element
    let style = set<IBig> "style"

    /// Specifies extra information about an element
    let title = set<IBig> "title"

    /// Specifies a language code for the content in an element, in  XHTML documents
    let xml_lang = set<IBig> "xml:lang"



module Blockquote =
    type IBlockquote = inherit IClosedElement
    let empty = tag "blockquote" : HtmlTag<IBlockquote>

    /// Specifies the source of the quotation
    let cite = set<IBlockquote> "cite"



module Body =
    type IBody = inherit IClosedElement
    let empty = tag "body" : HtmlTag<IBody>

    let appendSetUp f (x : HtmlTag<IBody>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLBodyElement> el)) x



module Br =
    type IBr = inherit IUnclosedElement
    let empty = unclosedTag "br" : HtmlTag<IBr>

    let appendSetUp f (x : HtmlTag<IBr>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLBRElement> el)) x



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

module Button =
    type IButton = inherit IClosedElement
    let empty = tag "button" : HtmlTag<IButton>

    let appendSetUp f (x : HtmlTag<IButton>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLButtonElement> el)) x

    /// Specifies that a button should automatically get focus when the page loads
    let autofocus = setEmpty<IButton> "autofocus"

    /// Specifies that a button should be disabled
    let disabled = setEmpty<IButton> "disabled"

    /// Specifies one or more forms the button belongs to
    let form = set<IButton> "form"

    /// Specifies where to send the form-data when a form is submitted. Only for type="submit"
    let formaction = set<IButton> "formaction"

    /// Specifies how form-data should be encoded before sending it to a server. Only for type="submit"
    let formenctype (x : Formenctype) = set<IButton> "formenctype" x.Value

    /// Specifies how to send the form-data (which HTTP method to use). Only for type="submit"
    let formmethod (x : Formmethod) = set<IButton> "formmethod" x.Value

    /// Specifies that the form-data should not be validated on submission. Only for type="submit"
    let formnovalidate = setEmpty<IButton> "formnovalidate"

    /// Specifies where to display the response after submitting the form. Only for type="submit"
    let formtarget (x : Target) = set<IButton> "formtarget" x.Value

    /// Specifies a name for the button
    let name = set<IButton> "name"

    /// Specifies the type of button
    let ``type`` (x : Type) = set<IButton> "type" x.Value

    /// Specifies an initial value for the button
    let value = set<IButton> "value"



module Canvas =
    type ICanvas = inherit IClosedElement
    let empty = tag "canvas" : HtmlTag<ICanvas>

    let appendSetUp f (x : HtmlTag<ICanvas>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLCanvasElement> el)) x

    /// Specifies the height of the canvas
    let height (x : int) = set<ICanvas> "height" (x.ToString())

    /// Specifies the width of the canvas
    let width (x : int) = set<ICanvas> "width" (x.ToString())



module Caption =
    type ICaption = inherit IUnclosedElement
    let empty = unclosedTag "caption" : HtmlTag<ICaption>



module Center =
    type ICenter = inherit IClosedElement
    let empty = tag "center" : HtmlTag<ICenter>

    /// Specifies a classname for an element
    let ``class`` = set<ICenter> "class"

    /// Specifies the text direction  for the content in an element
    let dir (x : DirType) = set<ICenter> "dir" x.Value

    /// Specifies a unique id for an element
    let id = set<ICenter> "id"

    /// Specifies a language code for the content in an element
    let lang = set<ICenter> "lang"

    /// Specifies an inline style for an element
    let style = set<ICenter> "style"

    /// Specifies extra information about an element
    let title = set<ICenter> "title"



module Cite =
    type ICite = inherit IClosedElement
    let empty = tag "cite" : HtmlTag<ICite>



module Code =
    type ICode = inherit IClosedElement
    let empty = tag "code" : HtmlTag<ICode>



module Col =
    type ICol = inherit IUnclosedElement
    let empty = unclosedTag "col" : HtmlTag<ICol>

    /// Specifies the number of columns a <col> element should span
    let span = set<ICol> "span"



module Colgroup =
    type IColgroup = inherit IClosedElement
    let empty = tag "colgroup" : HtmlTag<IColgroup>

    /// Specifies the number of columns a column group should span
    let span = set<IColgroup> "span"



type ICommandType =
    | Checkbox
    | Command
    | Radio
    member x.Value =
        match x with
        | Checkbox -> "checkbox"
        | Command -> "command"
        | Radio -> "radio"

module Command =
    type ICommand = inherit IClosedElement
    let empty = tag "command" : HtmlTag<ICommand>

    /// Specifies that the command should be checked when the page loads. Only  for type="radio" or type="checkbox"
    let ``checked`` = setEmpty<ICommand> "checked"

    /// Specifies that the command should be disabled
    let disabled = setEmpty<ICommand> "disabled"

    /// Specifies an image that represents the command
    let icon = set<ICommand> "icon"

    /// Required. Specifies the name of the command, as shown to the user
    let label = set<ICommand> "label"

    /// Specifies the name of the group of commands that will be toggled when the command itself is toggled.  Only for type="radio"
    let radiogroup = set<ICommand> "radiogroup"

    /// Specifies the type of command
    let ``type`` (x : ICommandType) = set<ICommand> "type" x.Value



module Datalist =
    type IDatalist = inherit IClosedElement
    let empty = tag "datalist" : HtmlTag<IDatalist>

    let appendSetUp f (x : HtmlTag<IDatalist>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLDataListElement> el)) x



module Dd =
    type IDd = inherit IClosedElement
    let empty = tag "dd" : HtmlTag<IDd>

    let appendSetUp f (x : HtmlTag<IDd>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLDDElement> el)) x



module Del =
    type IDel = inherit IClosedElement
    let empty = tag "del" : HtmlTag<IDel>

    /// Specifies a URL to a document that explains the reason why the text was deleted
    let cite = set<IDel> "cite"

    /// Specifies the date and time of when the text was deleted
    let datetime = set<IDel> "datetime"



module Details =
    type IDetails = inherit IClosedElement
    let empty = tag "details" : HtmlTag<IDetails>

    /// Specifies that the details should be visible (open) to the user
    let ``open`` = setEmpty<IDetails> "open"



module Dfn =
    type IDfn = inherit IClosedElement
    let empty = tag "dfn" : HtmlTag<IDfn>



module Dialog =
    type IDialog = inherit IClosedElement
    let empty = tag "dialog" : HtmlTag<IDialog>

    /// Specifies that the dialog element is active and that the user can  interact with it
    let ``open`` = setEmpty<IDialog> "open"



module Dir =
    type IDir = inherit IClosedElement
    let empty = tag "dir" : HtmlTag<IDir>

    /// Specifies a classname for an element
    let ``class`` = set<IDir> "class"

    /// Specifies the text direction  for the content in an element
    let dir (x : DirType) = set<IDir> "dir" x.Value

    /// Specifies a unique id for an element
    let id = set<IDir> "id"

    /// Specifies a language code for the content in an element
    let lang = set<IDir> "lang"

    /// Specifies an inline style for an element
    let style = set<IDir> "style"

    /// Specifies extra information about an element
    let title = set<IDir> "title"



module Div =
    type IDiv = inherit IClosedElement
    let empty = tag "div" : HtmlTag<IDiv>

    let appendSetUp f (x : HtmlTag<IDiv>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLDivElement> el)) x



module Dl =
    type IDl = inherit IClosedElement
    let empty = tag "dl" : HtmlTag<IDl>



module Dt =
    type IDt = inherit IClosedElement
    let empty = tag "dt" : HtmlTag<IDt>

    let appendSetUp f (x : HtmlTag<IDt>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLDTElement> el)) x



module Em =
    type IEm = inherit IClosedElement
    let empty = tag "em" : HtmlTag<IEm>



module Embed =
    type IEmbed = inherit IUnclosedElement
    let empty = unclosedTag "embed" : HtmlTag<IEmbed>

    let appendSetUp f (x : HtmlTag<IEmbed>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLEmbedElement> el)) x

    /// Specifies the height of the embedded content
    let height (x : int) = set<IEmbed> "height" (x.ToString())

    ///   Specifies the address of the external file to embed
    let src = set<IEmbed> "src"

    /// Specifies the MIME type of the embedded content
    let ``type`` = set<IEmbed> "type"

    /// Specifies the width of the embedded content
    let width (x : int) = set<IEmbed> "width" (x.ToString())



module Fieldset =
    type IFieldset = inherit IClosedElement
    let empty = tag "fieldset" : HtmlTag<IFieldset>

    let appendSetUp f (x : HtmlTag<IFieldset>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLFieldSetElement> el)) x

    /// Specifies that a group of related form elements should be disabled
    let disabled = setEmpty<IFieldset> "disabled"

    /// Specifies one or more forms the fieldset belongs to
    let form = set<IFieldset> "form"

    /// Specifies a name for the fieldset
    let name = set<IFieldset> "name"



module Figcaption =
    type IFigcaption = inherit IClosedElement
    let empty = tag "figcaption" : HtmlTag<IFigcaption>



module Figure =
    type IFigure = inherit IClosedElement
    let empty = tag "figure" : HtmlTag<IFigure>



module Font =
    type IFont = inherit IClosedElement
    let empty = tag "font" : HtmlTag<IFont>

    let appendSetUp f (x : HtmlTag<IFont>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLFontElement> el)) x

    /// Specifies a classname for an element
    let ``class`` = set<IFont> "class"

    /// Specifies the text direction  for the content in an element
    let dir (x : DirType) = set<IFont> "dir" x.Value

    /// Specifies a unique id for an element
    let id = set<IFont> "id"

    /// Specifies a language code for the content in an element
    let lang = set<IFont> "lang"

    /// Specifies an inline style for an element
    let style = set<IFont> "style"

    /// Specifies extra information about an element
    let title = set<IFont> "title"



module Footer =
    type IFooter = inherit IClosedElement
    let empty = tag "footer" : HtmlTag<IFooter>



type Autocomplete =
    | On
    | Off
    member x.Value =
        match x with
        | On -> "on"
        | Off -> "off"

type IFormTarget =
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

module Form =
    type IForm = inherit IClosedElement
    let empty = tag "form" : HtmlTag<IForm>

    let appendSetUp f (x : HtmlTag<IForm>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLFormElement> el)) x

    /// Specifies the character encodings that are to be used for the form  submission
    let accept_charset = set<IForm> "accept-charset"

    /// Specifies where to send the form-data when a form is submitted
    let action = set<IForm> "action"

    /// Specifies whether a form should have autocomplete on or off
    let autocomplete (x : Autocomplete) = set<IForm> "autocomplete" x.Value

    /// Specifies how the form-data should be encoded when submitting it to the  server (only for method="post")
    let enctype (x : Formenctype) = set<IForm> "enctype" x.Value

    /// Specifies the HTTP method to use when sending form-data
    let ``method`` (x : Formmethod) = set<IForm> "method" x.Value

    /// Specifies the name of a form
    let name = set<IForm> "name"

    /// Specifies that the form should not be validated when submitted
    let novalidate = setEmpty<IForm> "novalidate"

    /// Specifies where to display the response that is received after submitting the form
    let target (x : IFormTarget) = set<IForm> "target" x.Value



module Frame =
    type IFrame = inherit IUnclosedElement
    let empty = unclosedTag "frame" : HtmlTag<IFrame>

    let appendSetUp f (x : HtmlTag<IFrame>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLFrameElement> el)) x

    /// Specifies a classname for an element
    let ``class`` = set<IFrame> "class"

    /// Specifies a unique id for an element
    let id = set<IFrame> "id"

    /// Specifies an inline style for an element
    let style = set<IFrame> "style"

    /// Specifies extra information about an element
    let title = set<IFrame> "title"



module Frameset =
    type IFrameset = inherit IClosedElement
    let empty = tag "frameset" : HtmlTag<IFrameset>

    let appendSetUp f (x : HtmlTag<IFrameset>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLFrameSetElement> el)) x

    /// Specifies a classname for an element
    let ``class`` = set<IFrameset> "class"

    /// Specifies a unique id for an element
    let id = set<IFrameset> "id"

    /// Specifies an inline style for an element
    let style = set<IFrameset> "style"

    /// Specifies extra information about an element
    let title = set<IFrameset> "title"



module Head =
    type IHead = inherit IClosedElement
    let empty = tag "head" : HtmlTag<IHead>

    let appendSetUp f (x : HtmlTag<IHead>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLHeadElement> el)) x



module Header =
    type IHeader = inherit IClosedElement
    let empty = tag "header" : HtmlTag<IHeader>



module Hr =
    type IHr = inherit IUnclosedElement
    let empty = unclosedTag "hr" : HtmlTag<IHr>

    let appendSetUp f (x : HtmlTag<IHr>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLHRElement> el)) x



module Html =
    type IHtml = inherit IClosedElement
    let empty = tag "html" : HtmlTag<IHtml>

    let appendSetUp f (x : HtmlTag<IHtml>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLHtmlElement> el)) x

    /// Specifies the address of the document's cache manifest (for offline browsing)
    let manifest = set<IHtml> "manifest"



module I =
    type II = inherit IClosedElement
    let empty = tag "i" : HtmlTag<II>



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

module Iframe =
    type IIframe = inherit IClosedElement
    let empty = tag "iframe" : HtmlTag<IIframe>

    let appendSetUp f (x : HtmlTag<IIframe>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLIFrameElement> el)) x

    /// Specifies the height of an <iframe>
    let height (x : int) = set<IIframe> "height" (x.ToString())

    /// Specifies the name of an <iframe>
    let name = set<IIframe> "name"

    /// Enables a set of extra restrictions for the content in the <iframe>
    let sandbox (x : Sandbox) = set<IIframe> "sandbox" x.Value

    /// Specifies that the <iframe> should look like it is a part of the containing document
    let seamless = setEmpty<IIframe> "seamless"

    /// Specifies the address of the document to embed in the <iframe>
    let src = set<IIframe> "src"

    /// Specifies the HTML content of the page to show in the <iframe>
    let srcdoc = set<IIframe> "srcdoc"

    /// Specifies the width of an <iframe>
    let width (x : int) = set<IIframe> "width" (x.ToString())



type Crossorigin =
    | Anonymous
    | Use_credentials
    member x.Value =
        match x with
        | Anonymous -> "anonymous"
        | Use_credentials -> "use-credentials"

module Img =
    type IImg = inherit IUnclosedElement
    let empty = unclosedTag "img" : HtmlTag<IImg>

    /// Specifies an alternate text for an image
    let alt = set<IImg> "alt"

    /// Allow images from third-party sites that allow cross-origin access to be  used with canvas
    let crossorigin (x : Crossorigin) = set<IImg> "crossorigin" x.Value

    /// Specifies the height of an image
    let height (x : int) = set<IImg> "height" (x.ToString())

    /// Specifies an image as a server-side image-map
    let ismap = setEmpty<IImg> "ismap"

    /// Specifies the URL of an image
    let src = set<IImg> "src"

    /// Specifies an image as a client-side image-map
    let usemap = set<IImg> "usemap"

    /// Specifies the width of an image
    let width (x : int) = set<IImg> "width" (x.ToString())



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

type IInputType =
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

module Input =
    type IInput = inherit IUnclosedElement
    let empty = unclosedTag "input" : HtmlTag<IInput>

    let appendSetUp f (x : HtmlTag<IInput>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLInputElement> el)) x

    /// Specifies the types of files that the server accepts  (only for type="file")
    let accept (x : Accept) = set<IInput> "accept" x.Value

    /// Specifies an alternate text for images (only for type="image")
    let alt = set<IInput> "alt"

    /// Specifies whether an <input> element should have autocomplete  enabled
    let autocomplete (x : Autocomplete) = set<IInput> "autocomplete" x.Value

    /// Specifies that an <input> element should automatically get focus when the page  loads
    let autofocus = setEmpty<IInput> "autofocus"

    /// Specifies that an <input> element should be pre-selected when the page  loads (for type="checkbox" or type="radio")
    let ``checked`` = setEmpty<IInput> "checked"

    /// Specifies that an <input> element should be disabled
    let disabled = setEmpty<IInput> "disabled"

    /// Specifies one or more forms the <input> element belongs to
    let form = set<IInput> "form"

    /// Specifies the URL of the file that will process the input control when  the form is submitted (for type="submit" and type="image")
    let formaction = set<IInput> "formaction"

    /// Specifies how the form-data should be encoded when submitting it to the  server (for type="submit" and type="image")
    let formenctype (x : Formenctype) = set<IInput> "formenctype" x.Value

    /// Defines the HTTP  method for sending data to the action URL (for type="submit" and type="image")
    let formmethod (x : Formmethod) = set<IInput> "formmethod" x.Value

    /// Defines that form elements should not be validated when submitted
    let formnovalidate = setEmpty<IInput> "formnovalidate"

    /// Specifies where to display the response that is received after submitting  the form (for type="submit" and type="image")
    let formtarget (x : Target) = set<IInput> "formtarget" x.Value

    /// Specifies the height of an <input> element (only for type="image")
    let height (x : int) = set<IInput> "height" (x.ToString())

    /// Refers to a <datalist> element that contains pre-defined options for an  <input> element
    let list = set<IInput> "list"

    /// Specifies the maximum value for an <input> element
    let max (x : Max) = set<IInput> "max" x.Value

    /// Specifies the maximum number of characters allowed in an <input> element
    let maxlength = set<IInput> "maxlength"

    /// Specifies a minimum value for an <input> element
    let min (x : Max) = set<IInput> "min" x.Value

    /// Specifies that a user can enter more than one value in an <input>  element
    let multiple = setEmpty<IInput> "multiple"

    /// Specifies the name of an <input> element
    let name = set<IInput> "name"

    /// Specifies a regular expression that an <input> element's value is  checked against
    let pattern = set<IInput> "pattern"

    /// Specifies a short hint that describes the expected value of an <input>  element
    let placeholder = set<IInput> "placeholder"

    /// Specifies that an input field is read-only
    let ``readonly`` = setEmpty<IInput> "readonly"

    /// Specifies that an input field must be filled out before submitting the  form
    let required = setEmpty<IInput> "required"

    /// Specifies the width, in characters, of an <input> element
    let size = set<IInput> "size"

    /// Specifies the URL of the image to use as a submit button (only for  type="image")
    let src = set<IInput> "src"

    /// Specifies the legal number intervals for an input field
    let step = set<IInput> "step"

    /// Specifies the type <input> element to display
    let ``type`` (x : IInputType) = set<IInput> "type" x.Value

    /// Specifies the value of an <input> element   
    let value = set<IInput> "value"

    /// Specifies the width of an <input> element (only for type="image")
    let width (x : int) = set<IInput> "width" (x.ToString())



module Ins =
    type IIns = inherit IClosedElement
    let empty = tag "ins" : HtmlTag<IIns>

    /// Specifies a URL to a document that explains the reason why the text was  inserted/changed
    let cite = set<IIns> "cite"

    /// Specifies the date and time when the text was inserted/changed
    let datetime = set<IIns> "datetime"



module Kbd =
    type IKbd = inherit IClosedElement
    let empty = tag "kbd" : HtmlTag<IKbd>



type Keytype =
    | Rsa
    | Dsa
    | Ec
    member x.Value =
        match x with
        | Rsa -> "rsa"
        | Dsa -> "dsa"
        | Ec -> "ec"

module Keygen =
    type IKeygen = inherit IUnclosedElement
    let empty = unclosedTag "keygen" : HtmlTag<IKeygen>

    /// Specifies that a <keygen> element should automatically get focus when the page loads
    let autofocus = setEmpty<IKeygen> "autofocus"

    /// Specifies that the value of the <keygen> element should be challenged when submitted
    let challenge = setEmpty<IKeygen> "challenge"

    /// Specifies that a <keygen> element should be disabled
    let disabled = setEmpty<IKeygen> "disabled"

    /// Specifies one or more forms the <keygen> element belongs to
    let form = set<IKeygen> "form"

    /// Specifies the security algorithm of the key
    let keytype (x : Keytype) = set<IKeygen> "keytype" x.Value

    /// Defines a name for the <keygen> element
    let name = set<IKeygen> "name"



module Label =
    type ILabel = inherit IClosedElement
    let empty = tag "label" : HtmlTag<ILabel>

    let appendSetUp f (x : HtmlTag<ILabel>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLLabelElement> el)) x

    /// Specifies which form element a label is bound to
    let ``for`` = set<ILabel> "for"

    /// Specifies one or more forms the label belongs to
    let form = set<ILabel> "form"



module Legend =
    type ILegend = inherit IClosedElement
    let empty = tag "legend" : HtmlTag<ILegend>

    let appendSetUp f (x : HtmlTag<ILegend>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLLegendElement> el)) x



module Li =
    type ILi = inherit IClosedElement
    let empty = tag "li" : HtmlTag<ILi>

    let appendSetUp f (x : HtmlTag<ILi>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLLIElement> el)) x

    /// Specifies the value of a list item. The following list items will increment  from that number (only for <ol> lists)
    let value = set<ILi> "value"



type ILinkRel =
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

module Link =
    type ILink = inherit IUnclosedElement
    let empty = unclosedTag "link" : HtmlTag<ILink>

    let appendSetUp f (x : HtmlTag<ILink>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLLinkElement> el)) x

    /// Specifies the location of the linked document
    let href = set<ILink> "href"

    /// Specifies the language of the text in the linked document
    let hreflang = set<ILink> "hreflang"

    /// Specifies on what device the linked document will be displayed
    let media = set<ILink> "media"

    /// Required. Specifies the relationship between the current document and the linked  document
    let rel (x : ILinkRel) = set<ILink> "rel" x.Value

    /// Specifies the size of the linked resource. Only for rel="icon"
    let sizes (x : Sizes) = set<ILink> "sizes" x.Value

    /// Specifies the MIME type of the linked document
    let ``type`` = set<ILink> "type"



module Map =
    type IMap = inherit IClosedElement
    let empty = tag "map" : HtmlTag<IMap>

    let appendSetUp f (x : HtmlTag<IMap>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLMapElement> el)) x

    /// Required. Specifies the name of an image-map
    let name = set<IMap> "name"



module Mark =
    type IMark = inherit IClosedElement
    let empty = tag "mark" : HtmlTag<IMark>



type IMenuType =
    | Context
    | Toolbar
    | List
    member x.Value =
        match x with
        | Context -> "context"
        | Toolbar -> "toolbar"
        | List -> "list"

module Menu =
    type IMenu = inherit IClosedElement
    let empty = tag "menu" : HtmlTag<IMenu>

    let appendSetUp f (x : HtmlTag<IMenu>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLMenuElement> el)) x

    /// Specifies a visible label for the menu
    let label = set<IMenu> "label"

    /// Specifies which type of menu to display. Default value is "list"
    let ``type`` (x : IMenuType) = set<IMenu> "type" x.Value



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

module Meta =
    type IMeta = inherit IUnclosedElement
    let empty = unclosedTag "meta" : HtmlTag<IMeta>

    let appendSetUp f (x : HtmlTag<IMeta>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLMetaElement> el)) x

    /// Specifies the character encoding for the HTML document
    let charset = set<IMeta> "charset"

    /// Gives the value associated with the http-equiv or name attribute
    let content = set<IMeta> "content"

    /// Provides an HTTP header for the information/value of the content  attribute
    let http_equiv (x : Http_equiv) = set<IMeta> "http-equiv" x.Value

    /// Specifies a name for the metadata
    let name (x : Name) = set<IMeta> "name" x.Value



module Meter =
    type IMeter = inherit IClosedElement
    let empty = tag "meter" : HtmlTag<IMeter>

    /// Specifies one or more forms the <meter> element belongs to
    let form = set<IMeter> "form"

    /// Specifies the range that is considered to be a high value
    let high = set<IMeter> "high"

    /// Specifies the range that is considered to be a low value
    let low = set<IMeter> "low"

    /// Specifies the maximum value of the range
    let max = set<IMeter> "max"

    /// Specifies the minimum value of the range
    let min = set<IMeter> "min"

    /// Specifies what value is the optimal value for the gauge
    let optimum = set<IMeter> "optimum"

    /// Required. Specifies the current value of the gauge
    let value = set<IMeter> "value"



module Nav =
    type INav = inherit IClosedElement
    let empty = tag "nav" : HtmlTag<INav>



module Noframes =
    type INoframes = inherit IClosedElement
    let empty = tag "noframes" : HtmlTag<INoframes>

    /// Specifies a classname for an element
    let ``class`` = set<INoframes> "class"

    /// Specifies the text direction  for the content in an element
    let dir (x : DirType) = set<INoframes> "dir" x.Value

    /// Specifies a unique id for an element
    let id = set<INoframes> "id"

    /// Specifies a language code for the content in an element
    let lang = set<INoframes> "lang"

    /// Specifies an inline style for an element
    let style = set<INoframes> "style"

    /// Specifies extra information about an element
    let title = set<INoframes> "title"

    /// Specifies a language code for the content in an element, in  XHTML documents
    let xml_lang = set<INoframes> "xml:lang"



module Noscript =
    type INoscript = inherit IClosedElement
    let empty = tag "noscript" : HtmlTag<INoscript>



module Object =
    type IObject = inherit IClosedElement
    let empty = tag "object" : HtmlTag<IObject>

    let appendSetUp f (x : HtmlTag<IObject>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLObjectElement> el)) x

    /// Specifies the URL of the resource to be used by the object
    let data = set<IObject> "data"

    /// Specifies one or more forms the object belongs to
    let form = set<IObject> "form"

    /// Specifies the height of the object
    let height (x : int) = set<IObject> "height" (x.ToString())

    /// Specifies a name for the object
    let name = set<IObject> "name"

    /// Specifies the MIME type of data specified in the data attribute
    let ``type`` = set<IObject> "type"

    /// Specifies the name of a client-side image map to be used with the object
    let usemap = set<IObject> "usemap"

    /// Specifies the width of the object
    let width (x : int) = set<IObject> "width" (x.ToString())



type IOlType =
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

module Ol =
    type IOl = inherit IClosedElement
    let empty = tag "ol" : HtmlTag<IOl>

    /// Specifies that the list order should be descending (9,8,7...)
    let reversed = setEmpty<IOl> "reversed"

    /// Specifies the start value of an ordered list
    let start = set<IOl> "start"

    /// Specifies the kind of marker to use in the list
    let ``type`` (x : IOlType) = set<IOl> "type" x.Value



module Optgroup =
    type IOptgroup = inherit IClosedElement
    let empty = tag "optgroup" : HtmlTag<IOptgroup>

    let appendSetUp f (x : HtmlTag<IOptgroup>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLOptGroupElement> el)) x

    /// Specifies that an option-group should be disabled
    let disabled = setEmpty<IOptgroup> "disabled"

    /// Specifies a label for an option-group
    let label = set<IOptgroup> "label"



module Option =
    type IOption = inherit IClosedElement
    let empty = tag "option" : HtmlTag<IOption>

    let appendSetUp f (x : HtmlTag<IOption>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLOptionElement> el)) x

    /// Specifies that an option should be disabled
    let disabled = setEmpty<IOption> "disabled"

    /// Specifies a shorter label for an option
    let label = set<IOption> "label"

    /// Specifies that an option should be pre-selected when the page loads
    let selected = setEmpty<IOption> "selected"

    /// Specifies the value to be sent to a server
    let value = set<IOption> "value"



module Output =
    type IOutput = inherit IClosedElement
    let empty = tag "output" : HtmlTag<IOutput>

    /// Specifies the relationship between the result of the calculation, and the elements used in the calculation
    let ``for`` = set<IOutput> "for"

    /// Specifies one or more forms the output element belongs to
    let form = set<IOutput> "form"

    /// Specifies a name for the output element
    let name = set<IOutput> "name"



module P =
    type IP = inherit IClosedElement
    let empty = tag "p" : HtmlTag<IP>



module Param =
    type IParam = inherit IUnclosedElement
    let empty = unclosedTag "param" : HtmlTag<IParam>

    let appendSetUp f (x : HtmlTag<IParam>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLParamElement> el)) x

    /// Specifies the name of a parameter
    let name = set<IParam> "name"

    /// Specifies the value of the parameter
    let value = set<IParam> "value"



module Pre =
    type IPre = inherit IClosedElement
    let empty = tag "pre" : HtmlTag<IPre>

    let appendSetUp f (x : HtmlTag<IPre>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLPreElement> el)) x



module Progress =
    type IProgress = inherit IClosedElement
    let empty = tag "progress" : HtmlTag<IProgress>

    let appendSetUp f (x : HtmlTag<IProgress>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLProgressElement> el)) x

    /// Specifies how much work the task requires in total
    let max = set<IProgress> "max"

    /// Specifies how much of the task has been completed
    let value = set<IProgress> "value"



module Q =
    type IQ = inherit IClosedElement
    let empty = tag "q" : HtmlTag<IQ>

    /// Specifies the source URL of the quote
    let cite = set<IQ> "cite"



module Rp =
    type IRp = inherit IClosedElement
    let empty = tag "rp" : HtmlTag<IRp>



module Rt =
    type IRt = inherit IClosedElement
    let empty = tag "rt" : HtmlTag<IRt>



module Ruby =
    type IRuby = inherit IClosedElement
    let empty = tag "ruby" : HtmlTag<IRuby>



module S =
    type IS = inherit IClosedElement
    let empty = tag "s" : HtmlTag<IS>



module Samp =
    type ISamp = inherit IClosedElement
    let empty = tag "samp" : HtmlTag<ISamp>



module Script =
    type IScript = inherit IClosedElement
    let empty = tag "script" : HtmlTag<IScript>

    let appendSetUp f (x : HtmlTag<IScript>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLScriptElement> el)) x

    /// Specifies that the script is executed asynchronously (only for external scripts)
    let ``async`` = setEmpty<IScript> "async"

    /// Specifies the character encoding used in an external script  file
    let charset = set<IScript> "charset"

    /// Specifies that the script is executed when the page has finished parsing  (only for external scripts)
    let defer = setEmpty<IScript> "defer"

    /// Specifies the URL of an external script file
    let src = set<IScript> "src"

    /// Specifies the MIME type of the script
    let ``type`` = set<IScript> "type"



module Section =
    type ISection = inherit IClosedElement
    let empty = tag "section" : HtmlTag<ISection>



module Select =
    type ISelect = inherit IClosedElement
    let empty = tag "select" : HtmlTag<ISelect>

    let appendSetUp f (x : HtmlTag<ISelect>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLSelectElement> el)) x

    /// Specifies that the drop-down list should automatically get focus when  the page loads
    let autofocus = setEmpty<ISelect> "autofocus"

    /// Specifies that a drop-down list should be disabled
    let disabled = setEmpty<ISelect> "disabled"

    /// Defines one or more forms the select field belongs to
    let form = set<ISelect> "form"

    /// Specifies that multiple options can be selected at once
    let multiple = setEmpty<ISelect> "multiple"

    /// Defines a name for the drop-down list
    let name = set<ISelect> "name"

    /// Specifies that the user is required to select a value before submitting  the form
    let required = setEmpty<ISelect> "required"

    /// Defines the number of visible options in a drop-down list
    let size = set<ISelect> "size"



module Small =
    type ISmall = inherit IClosedElement
    let empty = tag "small" : HtmlTag<ISmall>



module Source =
    type ISource = inherit IUnclosedElement
    let empty = unclosedTag "source" : HtmlTag<ISource>

    let appendSetUp f (x : HtmlTag<ISource>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLSourceElement> el)) x

    /// Specifies the type of media resource
    let media = set<ISource> "media"

    /// Specifies the URL of the media file
    let src = set<ISource> "src"

    /// Specifies the MIME type of the media resource
    let ``type`` = set<ISource> "type"



module Span =
    type ISpan = inherit IClosedElement
    let empty = tag "span" : HtmlTag<ISpan>

    let appendSetUp f (x : HtmlTag<ISpan>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLSpanElement> el)) x



module Strike =
    type IStrike = inherit IClosedElement
    let empty = tag "strike" : HtmlTag<IStrike>

    /// Specifies a classname for an element
    let ``class`` = set<IStrike> "class"

    /// Specifies the text direction  for the content in an element
    let dir (x : DirType) = set<IStrike> "dir" x.Value

    /// Specifies a unique id for an element
    let id = set<IStrike> "id"

    /// Specifies a language code for the content in an element
    let lang = set<IStrike> "lang"

    /// Specifies an inline style for an element
    let style = set<IStrike> "style"

    /// Specifies extra information about an element
    let title = set<IStrike> "title"



module Strong =
    type IStrong = inherit IClosedElement
    let empty = tag "strong" : HtmlTag<IStrong>



type IStyleType =
    | Text_css
    member x.Value =
        match x with
        | Text_css -> "text/css"

module Style =
    type IStyle = inherit IClosedElement
    let empty = tag "style" : HtmlTag<IStyle>

    let appendSetUp f (x : HtmlTag<IStyle>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLStyleElement> el)) x

    /// Specifies what media/device the media resource is optimized for
    let media = set<IStyle> "media"

    /// Specifies that the styles only apply to this element's parent element  and that element's child elements
    let scoped = setEmpty<IStyle> "scoped"

    /// Specifies the MIME type of the style sheet
    let ``type`` (x : IStyleType) = set<IStyle> "type" x.Value



module Sub =
    type ISub = inherit IClosedElement
    let empty = tag "sub" : HtmlTag<ISub>



module Summary =
    type ISummary = inherit IClosedElement
    let empty = tag "summary" : HtmlTag<ISummary>



module Sup =
    type ISup = inherit IClosedElement
    let empty = tag "sup" : HtmlTag<ISup>



type Border =
    | Number of float
    | Empty
    member x.Value =
        match x with
        | Number number -> number.ToString()
        | Empty -> ""

module Table =
    type ITable = inherit IClosedElement
    let empty = tag "table" : HtmlTag<ITable>

    let appendSetUp f (x : HtmlTag<ITable>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLTableElement> el)) x

    /// Specifies whether the table cells should have borders or not
    let border (x : Border) = set<ITable> "border" x.Value



module Tbody =
    type ITbody = inherit IClosedElement
    let empty = tag "tbody" : HtmlTag<ITbody>



module Td =
    type ITd = inherit IClosedElement
    let empty = tag "td" : HtmlTag<ITd>

    /// Specifies the number of columns a cell should span
    let colspan = set<ITd> "colspan"

    /// Specifies one or more header cells a cell is related to
    let headers = set<ITd> "headers"

    /// Sets the number of rows a cell should span
    let rowspan = set<ITd> "rowspan"



type Wrap =
    | Hard
    | Soft
    member x.Value =
        match x with
        | Hard -> "hard"
        | Soft -> "soft"

module Textarea =
    type ITextarea = inherit IUnclosedElement
    let empty = unclosedTag "textarea" : HtmlTag<ITextarea>

    let appendSetUp f (x : HtmlTag<ITextarea>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLTextAreaElement> el)) x

    /// Specifies that a text area should automatically get focus when the page  loads
    let autofocus = setEmpty<ITextarea> "autofocus"

    /// Specifies the visible width of a text area
    let cols = set<ITextarea> "cols"

    /// Specifies that a text area should be disabled
    let disabled = setEmpty<ITextarea> "disabled"

    /// Specifies one or more forms the text area belongs to
    let form = set<ITextarea> "form"

    /// Specifies the maximum number of characters allowed in the text area
    let maxlength = set<ITextarea> "maxlength"

    /// Specifies a name for a text area
    let name = set<ITextarea> "name"

    /// Specifies a short hint that describes the expected value of a text area
    let placeholder = set<ITextarea> "placeholder"

    /// Specifies that a text area should be read-only
    let ``readonly`` = setEmpty<ITextarea> "readonly"

    /// Specifies that a text area is required/must be filled out
    let required = setEmpty<ITextarea> "required"

    /// Specifies the visible number of lines in a text area
    let rows = set<ITextarea> "rows"

    /// Specifies how the text in a text area is to be wrapped when submitted in a form
    let wrap (x : Wrap) = set<ITextarea> "wrap" x.Value



module Tfoot =
    type ITfoot = inherit IClosedElement
    let empty = tag "tfoot" : HtmlTag<ITfoot>



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

module Th =
    type ITh = inherit IClosedElement
    let empty = tag "th" : HtmlTag<ITh>

    /// Specifies the number of columns a header cell should span
    let colspan = set<ITh> "colspan"

    /// Specifies one or more header cells a cell is related to
    let headers = set<ITh> "headers"

    /// Specifies the number of rows a header cell should span
    let rowspan = set<ITh> "rowspan"

    /// Specifies whether a header cell is a header for a column, row, or group  of columns or rows
    let scope (x : Scope) = set<ITh> "scope" x.Value



module Thead =
    type IThead = inherit IClosedElement
    let empty = tag "thead" : HtmlTag<IThead>



module Time =
    type ITime = inherit IClosedElement
    let empty = tag "time" : HtmlTag<ITime>

    /// Gives the date/time being specified. Otherwise, the date/time is given  by the element's contents
    let datetime = set<ITime> "datetime"



module Title =
    type ITitle = inherit IClosedElement
    let empty = tag "title" : HtmlTag<ITitle>

    let appendSetUp f (x : HtmlTag<ITitle>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLTitleElement> el)) x



module Tr =
    type ITr = inherit IClosedElement
    let empty = tag "tr" : HtmlTag<ITr>



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

module Track =
    type ITrack = inherit IUnclosedElement
    let empty = unclosedTag "track" : HtmlTag<ITrack>

    let appendSetUp f (x : HtmlTag<ITrack>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLTrackElement> el)) x

    /// Specifies that the track is to be enabled if the user's preferences do  not indicate that another track would be more appropriate
    let ``default`` = setEmpty<ITrack> "default"

    /// Specifies the kind of text track
    let kind (x : Kind) = set<ITrack> "kind" x.Value

    /// Specifies the title of the text track
    let label = set<ITrack> "label"

    /// Required. Specifies the URL of the track file
    let src = set<ITrack> "src"

    /// Specifies the language of the track text data (required if kind="subtitles")
    let srclang = set<ITrack> "srclang"



module Tt =
    type ITt = inherit IClosedElement
    let empty = tag "tt" : HtmlTag<ITt>

    /// Specifies a classname for an element
    let ``class`` = set<ITt> "class"

    /// Specifies the text direction for the content in an element
    let dir (x : DirType) = set<ITt> "dir" x.Value

    /// Specifies a unique id for an element
    let id = set<ITt> "id"

    /// Specifies a language code for the content in an element
    let lang = set<ITt> "lang"

    /// Specifies an inline style for an element
    let style = set<ITt> "style"

    /// Specifies extra information about an element
    let title = set<ITt> "title"

    /// Specifies a language code for the content in an element, in  XHTML documents
    let xml_lang = set<ITt> "xml:lang"



module U =
    type IU = inherit IClosedElement
    let empty = tag "u" : HtmlTag<IU>



module Ul =
    type IUl = inherit IClosedElement
    let empty = tag "ul" : HtmlTag<IUl>



module Var =
    type IVar = inherit IClosedElement
    let empty = tag "var" : HtmlTag<IVar>



module Video =
    type IVideo = inherit IClosedElement
    let empty = tag "video" : HtmlTag<IVideo>

    let appendSetUp f (x : HtmlTag<IVideo>) =
        Element.appendSetUp (fun el -> f(unbox<HTMLVideoElement> el)) x

    /// Specifies that the video will start playing as soon as it is ready
    let autoplay = setEmpty<IVideo> "autoplay"

    /// Specifies that video controls should be displayed (such as a play/pause button etc).
    let controls = setEmpty<IVideo> "controls"

    /// Sets the height of the video player
    let height (x : int) = set<IVideo> "height" (x.ToString())

    /// Specifies that the video will start over again, every time it is finished
    let loop = setEmpty<IVideo> "loop"

    /// Specifies that the audio output of the video should be muted
    let muted = setEmpty<IVideo> "muted"

    /// Specifies an image to be shown while the video is downloading, or until the user hits the play button
    let poster = set<IVideo> "poster"

    /// Specifies if and how the author thinks the video should be loaded when the page loads
    let preload (x : Preload) = set<IVideo> "preload" x.Value

    /// Specifies the URL of the video file
    let src = set<IVideo> "src"

    /// Sets the width of the video player
    let width (x : int) = set<IVideo> "width" (x.ToString())



module Wbr =
    type IWbr = inherit IUnclosedElement
    let empty = unclosedTag "wbr" : HtmlTag<IWbr>