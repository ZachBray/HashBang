[<FunScript.JS>]
module TypeInferred.HashBang.Bootstrap.Inputs

open FunScript.TypeScript
open TypeInferred.HashBang.Html
open TypeInferred.HashBang.Bootstrap.Stylesheets

let labelSpace = Bootstrap.col_xs_2
let labelOffset = Bootstrap.col_xs_offset_2
let formSpace = Bootstrap.col_xs_10

/// Adds a placeholder to an input element
let withPlaceholder placeholderText f =
    f |> FormQuery.mapElements (fun elements context ->
        elements |> Array.map (fun element ->
            element |> Input.placeholder placeholderText))

/// Adds an error tooltip to an input element
let withErrorTooltip f =
    f |> FormQuery.mapElements (fun elements context ->
        elements |> Array.map (fun element ->
            element |> Element.appendSetUpByJQuery (fun field ->
                context.SubscribeLocalValue (function
                    | Invalid(FieldError msg) -> field.tooltip("destroy").data("title", msg).tooltip() |> ignore
                    | _ -> field.data("title", "").tooltip("destroy") |> ignore)
            )))

/// Wraps an input element in a form group with a label
let withLabel labelText f =
    f |> FormQuery.mapElements (fun elements _ ->
        elements |> Array.map (fun (element : HtmlTag<_,_>) ->
            div [Bootstrap.form_group] [
                label [Bootstrap.control_label; labelSpace] []
                |> Label.``for`` element.Id
                |> Element.appendText [labelText]
                :> IHtmlTag

                div [formSpace] [
                    element
                    |> Element.appendClass Bootstrap.form_control
                    :> IHtmlTag
                ] :> IHtmlTag
            ]
        ))

/// Adds error and success feedback to a form group
let withFeedback hasSuccessFeedback f =
    f |> FormQuery.mapElements (fun elements context ->
        elements |> Array.map (fun element ->
            element |> Element.appendSetUpByJQuery (fun formGroup ->
                context.SubscribeLocalValue (function
                    | Invalid(FieldError _) -> 
                        formGroup.removeClass(Bootstrap.has_success).addClass(Bootstrap.has_error) |> ignore
                    | Invalid Unset -> 
                        formGroup.removeClass(Bootstrap.has_error)
                                 .removeClass(Bootstrap.has_success) |> ignore
                    | Valid _ -> 
                        formGroup.removeClass(Bootstrap.has_error) |> ignore
                        if hasSuccessFeedback then
                            formGroup.addClass(Bootstrap.has_success) |> ignore)
            )))

/// Adds errorfeedback to a form group
let withErrorFeedback f = withFeedback false f

/// Adds error and success feedback to a form group
let withSuccessAndErrorFeedback f = withFeedback true f

/// Adds success and error icon feedback to a form group
let withErrorIconFeedback f =
    f |> FormQuery.mapElements (fun elements context ->
        elements |> Array.map (fun element ->
            element 
            |> Element.appendClass Bootstrap.has_feedback
            |> Element.appendTags [
                span [] []
                |> Element.appendSetUpByJQuery (fun span ->
                    context.SubscribeLocalValue (function
                        | Invalid Unset ->
                            span.removeClass(Bootstrap.glyphicon)
                                .removeClass(Bootstrap.glyphicon_remove)
                                .removeClass(Bootstrap.glyphicon_ok)
                                .removeClass(Bootstrap.form_control_feedback) 
                            |> ignore
                        | Invalid(FieldError _) ->
                            span.addClass(Bootstrap.glyphicon)
                                .removeClass(Bootstrap.glyphicon_ok)
                                .addClass(Bootstrap.glyphicon_remove)
                                .addClass(Bootstrap.form_control_feedback) 
                            |> ignore
                        | Valid _ ->
                            span.addClass(Bootstrap.glyphicon)
                                .removeClass(Bootstrap.glyphicon_remove)
                                .addClass(Bootstrap.glyphicon_ok)
                                .addClass(Bootstrap.form_control_feedback) 
                            |> ignore))
            ]))

/// Creates a text input with the label and placeholder text provided.
let text labelText placeholderText validate = 
    InputFactories.text []
    |> withPlaceholder placeholderText
    |> validate
    |> withErrorTooltip
    |> withLabel labelText
    |> withSuccessAndErrorFeedback
    |> withErrorIconFeedback

/// Creates a password input with the label and placeholder text provided.
let password labelText placeholderText  validate=
    InputFactories.password []
    |> withPlaceholder placeholderText
    |> validate
    |> withErrorTooltip
    |> withLabel labelText
    |> withSuccessAndErrorFeedback
    |> withErrorIconFeedback

/// Creates a drop-down input with the label and values provided.
/// Note: the value type must have a .ToString() method implemented
let select labelText values validate =
    InputFactories.select [] values
    |> validate
    |> withErrorTooltip
    |> withLabel labelText
    |> withSuccessAndErrorFeedback
    |> withErrorIconFeedback

/// Creates a checkbox with the text provided as a label.
let checkbox text validate =
    InputFactories.checkbox [] [] text
    |> validate
    |> FormQuery.mapElements (fun elements _ -> 
        [|
            div [Bootstrap.form_group] (elements 
                    |> Array.map (fun element -> 
                        div [labelOffset; formSpace] [
                            div [Bootstrap.checkbox] [element] 
                        ]:> IHtmlTag)
                    |> Array.toList)
        |])
    |> withErrorFeedback
    |> withErrorTooltip

/// Creates a radio group with the values provided as the options.
/// Note: the value type must have a .ToString() method implemented
let radioGroup values validate =
    InputFactories.radioGroup [] [] values
    |> validate
    |> FormQuery.mapElements (fun elements _ -> 
        [|
            div [Bootstrap.form_group] (elements 
                    |> Array.map (fun element -> 
                        div [labelOffset; formSpace] [
                            div [Bootstrap.radio] [element] 
                        ]:> IHtmlTag)
                    |> Array.toList)
        |])
    |> withErrorFeedback
    |> withErrorTooltip

/// Creates a horizontal (inline) radio group with the values provided as the options.
/// Note: the value type must have a .ToString() method implemented
let radioGroupInline values validate =
    InputFactories.radioGroup [Bootstrap.radio_inline] [] values
    |> validate
    |> FormQuery.mapElements (fun elements _ -> 
        [| div [labelOffset; formSpace] (elements |> Array.toList) |])
    |> withErrorFeedback
    |> withErrorTooltip