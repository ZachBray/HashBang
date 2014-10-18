[<ReflectedDefinition>]
module TypeInferred.HashBang.Html.Compiler

open TypeInferred.HashBang.Html.Tags

module Helpers =
    let indent xs = xs |> Seq.map (fun x -> "  " + x)
       
    let attributes xs =
        xs |> Map.toSeq |> Seq.map (fun (k, v) -> 
            match v with
            | None -> " " + k
            | Some v -> " " + k + "=\"" + v + "\"")
        |> String.concat ""

    let openTag (tag : IHtmlTag) =
        "<" + tag.Name + " id=\"" + tag.Id + "\"" + attributes tag.Attributes + ">"

    let closingTag (tag : IHtmlTag) = 
        "</" + tag.Name + ">"

    let rec compileTag (tag : IHtmlTag) =
        seq {
            if tag.IsVoid then
                yield openTag tag
            else
                yield openTag tag 
                yield! tag.Children |> Seq.collect compileChild |> indent
                yield closingTag tag
        }

    and compileChild = function
        | Text str -> Seq.singleton str
        | Tag t -> compileTag t

let compilePage (html : HtmlTag<IHtmlElement,_>) =
    seq {
        yield "<!DOCTYPE html>"
        yield! Helpers.compileTag html
    } |> String.concat "\r\n"

let compilePageUnsafe (html : IHtmlTag) =
    seq {
        yield "<!DOCTYPE html>"
        yield! Helpers.compileTag html
    } |> String.concat "\r\n"

let compileSection tag =
    Helpers.compileTag tag |> String.concat "\r\n"

let rec initialize (tag : IHtmlTag) =
    let resource = tag.Initialize tag.Id
    let childResources =
        tag.Children |> List.choose (function
            | Text _ -> None
            | Tag t -> Some(initialize t))
        |> List.toArray
        |> Disposable.combine
    Disposable.combine [|resource; childResources|]