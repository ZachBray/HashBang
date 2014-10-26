[<FunScript.JS; RequireQualifiedAccess>]
module TypeInferred.HashBang.Html.InputFactories

open System
open FunScript
open FunScript.TypeScript

let checkbox labelClasses checkClasses text =  
    FormQuery.fromContext (fun context ->
        [|
            label labelClasses [
                input checkClasses
                |> Input.``type`` IInputElementType.Checkbox
                |> Element.appendSetUp (fun field ->
                    let sendValue() = context.OnValidated(field._checked)
                    field.onchange <- fun _ -> sendValue(); null
                    sendValue()
                    Disposable.by (fun () -> field.onchange <- null))
            ] |> Element.appendText [text]
        |])

let radioGroup labelClasses radioClasses values = 
    FormQuery.fromContext (fun context ->
        let groupName = Unchecked.randomId()
        values |> List.map (fun v ->
            label labelClasses [
                input radioClasses
                |> Input.name groupName
                |> Input.``type`` IInputElementType.Radio
                |> Element.appendSetUp (fun field ->
                    let sendValue() = 
                        if field._checked then context.OnValidated v
                    field.onchange <- fun _ -> sendValue(); null
                    Disposable.by (fun () -> field.onchange <- null))
            ] |> Element.appendText [v.ToString()]
            :> IHtmlTag
        ) |> List.toArray)

let input inputType classes =
    FormQuery.fromContext (fun context ->
        [|
            input classes
            |> Element.appendSetUp (fun field ->
                let sendValue() = context.OnValidated(field.value)
                field.onkeyup <- fun _ -> sendValue(); null
                field.onpaste <- fun _ -> sendValue(); null
                field.oncut <- fun _ -> sendValue(); null
                field.onchange <- fun _ -> sendValue(); null
                sendValue()
                Disposable.by (fun () -> 
                    field.onkeyup <- null
                    field.onpaste <- null
                    field.oncut <- null
                    field.onchange <- null))
            |> Input.``type`` inputType
        |])
        
let text classes = input IInputElementType.Text_ classes 

let password classes = input IInputElementType.Password classes

let textareaFactory classes =
    FormQuery.fromContext (fun context ->
        [|
            textarea classes []
            |> Element.appendSetUp (fun field ->
                let sendValue() = context.OnValidated(field.value)
                field.onkeyup <- fun _ -> sendValue(); null
                field.onpaste <- fun _ -> sendValue(); null
                field.oncut <- fun _ -> sendValue(); null
                field.onchange <- fun _ -> sendValue(); null
                sendValue()
                Disposable.by (fun () -> 
                    field.onkeyup <- null
                    field.onpaste <- null
                    field.oncut <- null
                    field.onchange <- null))
        |])

let select classes values =
    FormQuery.fromContext (fun context ->
        let options = 
            values |> List.mapi (fun i value -> 
            option [] [] 
            |> Element.appendText [value.ToString()] 
            |> Option.value (i.ToString())
            :> IHtmlTag)
        let values = values |> List.toArray
        [|
            select classes options
            |> Element.appendSetUp (fun field ->
                field.onchange <- fun _ ->
                    match Int32.TryParseJS field.value with
                    | None -> context.OnInvalidated Unset
                    | Some index -> context.OnValidated values.[index]
                    null
                field.value <- "0"
                Disposable.by (fun () -> field.onchange <- null))
        |])