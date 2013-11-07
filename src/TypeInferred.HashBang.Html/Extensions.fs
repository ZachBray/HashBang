[<AutoOpen; ReflectedDefinition>]
module TypeInferred.HashBang.Html.Extensions

open TypeInferred.HashBang.Html

type JQuery with
    member jq.html(tag : IHtmlTag) =
        let text = Compiler.compileSection tag
        let acc = jq.html(text)
        Compiler.initialize tag
        acc