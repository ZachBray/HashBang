[<FunScript.JS>]
module TypeInferred.HashBang.Bootstrap.FormQuery

open FunScript.TypeScript
open TypeInferred.HashBang.Html
open TypeInferred.HashBang.Bootstrap.Stylesheets

module Components =
    let progessBar(context : FormContext<_>) =
        div [Bootstrap.progress] [
            div [Bootstrap.progress_bar; Bootstrap.progress_bar_striped; Bootstrap.active] []
            |> Unchecked.set "role" "progressbar"
            |> Unchecked.set "aria-valuenow" "100"
            |> Unchecked.set "aria-valuemin" "0"
            |> Unchecked.set "aria-valuemax" "100"
            |> Element.style "width: 100%;"
        ]
        |> Element.hidden
        |> Element.appendSetUp (fun progress ->
            context.SubscribeFormStatus (function
                | Active -> progress.hidden <- true
                | Disabled -> progress.hidden <- false))

/// Adds a submit button that performs an async action when the form produces a valid value.
let withSubmitButton label onSubmit f =
    f |> FormQuery.mapElements (fun elements context ->
        [|
            yield! elements |> Array.map (fun tag -> tag :> IHtmlTag)
            yield div [Bootstrap.form_group] [ 
                div [Inputs.labelOffset; Inputs.formSpace] [
                    button [Bootstrap.btn; Bootstrap.btn_default] [] 
                    |> Button.``type`` Type.Button
                    |> Element.appendText [label]
                    |> Element.appendTags [
                        span [] [
                            i [FontAwesome.fa; FontAwesome.fa_circle_o_notch; FontAwesome.fa_spin] []
                        ]
                        |> Element.hidden
                        |> Element.appendSetUp (fun icon ->
                            context.SubscribeFormStatus (function
                                | Active -> icon.hidden <- true
                                | Disabled -> icon.hidden <- false)
                        )
                    ]
                    |> Element.appendSetUp (fun button ->
                        context.SubscribeLocalValue (function
                        | Valid x -> 
                            button.onclick <- fun _ ->
                                async {
                                    context.OnStatusChanged Disabled
                                    try do! onSubmit x
                                    with ex -> Globals.alert(ex.Message)
                                    context.OnStatusChanged Active
                                } |> Async.StartImmediate
                                null
                            button.disabled <- false
                            ()
                        | Invalid _ -> 
                            button.disabled <- true
                            button.onclick <- null))
                ]
            ]
            :> IHtmlTag
        |])

/// Adds an indeterminate progress bar that is shown whilst the form is in a disabled state.
let withProgressBar f =
    f |> FormQuery.mapElements (fun elements context ->
        [|
            yield! elements |> Array.map (fun tag -> tag :> IHtmlTag)
            yield div [Bootstrap.form_group] [
                div [Inputs.labelOffset; Inputs.formSpace] [
                    Components.progessBar context
                ] 
            ]:> IHtmlTag
        |])

/// Disables all parts of the form whilst it is in a Disabled state. E.g., whilst it
/// is being submitted.
let withDisablingFieldset f =
    f |> FormQuery.mapElements (fun elements context ->
        [|
            fieldset [] (elements |> Array.toList |> List.map (fun tag -> tag :> IHtmlTag))
            |> Element.appendSetUp (fun fieldset ->
                context.SubscribeFormStatus (function
                    | Active -> fieldset.disabled <- false
                    | Disabled -> fieldset.disabled <- true))
        |])


let runBootstrap f =
    FormQuery.run f
    |> Element.appendClass Bootstrap.form_horizontal