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

    let setEmpty<'a> n t =
        { t with Attributes = t.Attributes |> Map.add n None } : HtmlTag<'a>
        

open Unchecked

type IClosedElement = interface end
type IUnclosedElement = interface end
module Element =
    let append<'a when 'a :> IClosedElement> xs (x : HtmlTag<'a>) =
        { x with Children = x.Children @ xs }