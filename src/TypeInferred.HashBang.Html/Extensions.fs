[<AutoOpen; ReflectedDefinition>]
module TypeInferred.HashBang.Html.Extensions

open FunScript.TypeScript
open TypeInferred.HashBang.Html

type JQuery with
    member jq.html(tag : IHtmlTag) =
        let text = Compiler.compileSection tag
        let acc = jq.html(text)
        Compiler.initialize tag

    member jq.append(tag : IHtmlTag) =
        let text = Compiler.compileSection tag
        let acc = jq.append(text)
        Compiler.initialize tag
        
    member jq.prepend(tag : IHtmlTag) =
        let text = Compiler.compileSection tag
        let acc = jq.prepend(text)
        Compiler.initialize tag

    member jq.after(tag : IHtmlTag) =
        let text = Compiler.compileSection tag
        let acc = jq.after(text)
        Compiler.initialize tag
        
    member jq.before(tag : IHtmlTag) =
        let text = Compiler.compileSection tag
        let acc = jq.before(text)
        Compiler.initialize tag

let (==>) x ys = x |> Element.appendTags ys

let (-->) x y = x |> Element.appendText [y]

let (+.) x ys = x |> Element.classes ys