[<FunScript.JS>]
module TypeInferred.HashBang.Bootstrap.Modal

open FunScript.TypeScript
open TypeInferred.HashBang.Html
open Stylesheets

type ModalContext = { Close : unit -> unit }

type ModalSize = Small | Medium | Large

type ModalDescription =
    {
        Size : ModalSize
        HeaderFactory : ModalContext -> IHtmlTag list
        BodyFactory : ModalContext -> IHtmlTag list
        FooterFactory : ModalContext -> IHtmlTag list
    }
    static member Empty = 
        { 
            Size = Medium
            HeaderFactory = fun _ -> []
            BodyFactory = fun _ -> []
            FooterFactory = fun _ -> [] 
        }

    static member Titled title =
        {
            ModalDescription.Empty with
                HeaderFactory = fun _ ->
                    [h4 [Bootstrap.modal_title] [] |> Element.appendText[title] :> IHtmlTag]
        }

    static member TitledWithClose title =
        let titledDescription = ModalDescription.Titled title
        {   titledDescription with
                HeaderFactory = fun context -> 
                    [
                        yield upcast (
                            button [Bootstrap.close] [] 
                            |> Button.``type`` Type.Button
                            |> Element.onclick (fun _ -> context.Close())
                            |> Element.appendTags [
                                span [] [] |> Element.appendText ["&times;"] |> Unchecked.set "aria-hidden" "true"
                                span [Bootstrap.sr_only] [] |> Element.appendText ["Close"]
                            ])
                        yield! titledDescription.HeaderFactory context
                    ]
        }

let show (description : ModalDescription) =
    let modalElement = 
        div [Bootstrap.modal; Bootstrap.fade] []
        |> Unchecked.set "role" "dialog"
        |> Unchecked.set "aria-hidden" "true"
        |> Element.tabindex -1
        |> ref

    let resources = lazy Globals.jQuery.Invoke("body").append(!modalElement)
    let modalJQuery() = Globals.jQuery.Invoke("#" + (!modalElement).Id)

    let context =
        {
            Close = fun () -> 
                resources.Value.Dispose()
                modalJQuery().removeClass(Bootstrap.fade).modal("hide").remove() |> ignore
        }

    modalElement :=
        !modalElement
        |> Element.appendTags [
            div [
                yield Bootstrap.modal_dialog 
                match description.Size with
                | Small -> yield Bootstrap.modal_sm
                | Large -> yield Bootstrap.modal_lg
                | Medium -> ()
            ] [
                div [Bootstrap.modal_content] [
                    match description.HeaderFactory context with
                    | [] -> ()
                    | header -> yield upcast (div [Bootstrap.modal_header] [] |> Element.appendTags header)
                    match description.BodyFactory context with
                    | [] -> ()
                    | body -> yield upcast (div [Bootstrap.modal_body] [] |> Element.appendTags body)
                    match description.FooterFactory context with
                    | [] -> ()
                    | footer -> yield upcast (div [Bootstrap.modal_footer] [] |> Element.appendTags footer)
                ]
            ]
        ] 

    resources.Value |> ignore
    modalJQuery().modal("show") |> ignore