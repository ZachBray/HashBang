[<AutoOpen; ReflectedDefinition>]
module TypeInferred.HashBang.Html.Tags
#if INTERACTIVE
#r @"..\packages\FunScript.1.1.0.25\lib\net40\FunScript.dll"
#r @"..\packages\FunScript.1.1.0.25\lib\net40\FunScript.Interop.dll"
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