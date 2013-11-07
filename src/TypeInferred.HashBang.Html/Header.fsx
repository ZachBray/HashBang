[<ReflectedDefinition>]
module TypeInferred.HashBang.Html

type IHtmlTag =
    abstract Name : string
    abstract Attributes : Map<string, string option>
    abstract Children : IHtmlTag list
    abstract CanClose : bool


type HtmlTag<'a> =
    {
        Name : string
        Attributes : Map<string, string option>
        Children : IHtmlTag list
        CanClose : bool
    }

    interface IHtmlTag with
        member tag.Name = tag.Name
        member tag.Attributes = tag.Attributes
        member tag.Children = tag.Children
        member tag.CanClose = tag.CanClose

module Unchecked =
    let tag name =
        {
            Name = name
            Attributes = Map.empty
            Children = []
            CanClose = true
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
    /// Appends children to the element
    let append<'a when 'a :> IClosedElement> xs (x : HtmlTag<'a>) =
        { x with Children = x.Children @ xs }

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
    let id v = set "id" v

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