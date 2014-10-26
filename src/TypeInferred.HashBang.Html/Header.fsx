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

    let addAttributeValue<'TElement, 'TTypeScriptElement> n v (t : HtmlTag<'TElement, 'TTypeScriptElement>) = 
        let updatedAttributes =
            match t.Attributes.TryFind n with
            | None -> t.Attributes |> Map.add n (Some v)
            | Some None -> failwith "Unexpected attribute type"
            | Some(Some oldV) -> t.Attributes |> Map.add n (Some(oldV + " " + v))
        { t with Attributes = updatedAttributes } : HtmlTag<'TElement, 'TTypeScriptElement>

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

    /// Specifies one or more classnames for an element (refers to a class in a style sheet)
    let appendClass v = addAttributeValue "class" v

    /// Specifies one or more classnames for an element (refers to a class in a style sheet)
    let appendClasses vs = addAttributeValue "class" (vs |> String.concat " ")

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