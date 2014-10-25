[<FunScript.JS; RequireQualifiedAccess>]
module TypeInferred.HashBang.Html.InputFactories

open System
open FunScript
open FunScript.TypeScript

let input style filter =
    FormQuery.delay(fun () ->
        let stateObservers = ResizeArray()
        FormQuery.fromFunctions (fun onValid _ ->
            input []
            |> Element.appendSetUp (fun field ->
                let sendValue() = onValid(field.value)
                field.onkeydown <- fun _ -> sendValue(); null
                field.onpaste <- fun _ -> sendValue(); null
                field.oncut <- fun _ -> sendValue(); null
                field.onchange <- fun _ -> sendValue(); null
                null)
            |> style (fun f -> 
                stateObservers.Add f
                Disposable.by (fun () -> stateObservers.Remove f |> ignore)))
        |> filter
        |> FormQuery.tapValue (fun x ->
            stateObservers |> Seq.iter (fun f -> f x)))
            // TODO: Would it be better just to walk the elements and apply
            //       the appropriate validation feedback.
            //       i.e., formQuery |> Validation.showErrors
        
let textarea style filter =
    FormQuery.delay(fun () ->
        let stateObservers = ResizeArray()
        FormQuery.fromFunctions (fun onValid _ ->
            textarea [] []
            |> Element.appendSetUp (fun field ->
                let sendValue() = onValid(field.value)
                field.onkeydown <- fun _ -> sendValue(); null
                field.onpaste <- fun _ -> sendValue(); null
                field.oncut <- fun _ -> sendValue(); null
                field.onchange <- fun _ -> sendValue(); null
                null)
            |> style (fun f -> 
                stateObservers.Add f
                Disposable.by (fun () -> stateObservers.Remove f |> ignore)))
        |> filter
        |> FormQuery.tapValue (fun x ->
            stateObservers |> Seq.iter (fun f -> f x)))

let text style = 
    input (fun subscribeToState element -> 
        element 
        |> Input.``type`` IInputElementType.Text_ 
        |> style subscribeToState)

let password style = 
    input (fun subscribeToState element -> 
        element 
        |> Input.``type`` IInputElementType.Password 
        |> style subscribeToState)

let select style values filter =
    let options = 
        values |> List.mapi (fun i value -> 
        option [] [] 
        |> Element.appendText [value.ToString()] 
        |> Option.value (i.ToString())
        :> IHtmlTag)
    let values = values |> List.toArray
    FormQuery.delay(fun () ->
        let stateObservers = ResizeArray()
        FormQuery.fromFunctions (fun onValid onInvalid ->
            select [] options
            |> Element.appendSetUp (fun field ->
                field.onchange <- fun _ ->
                    match Int32.TryParseJS field.value with
                    | None -> onInvalid Unset
                    | Some index -> onValid values.[index]
                    null
                null)
            |> style (fun f -> 
                stateObservers.Add f
                Disposable.by (fun () -> stateObservers.Remove f |> ignore)))
        |> filter
        |> FormQuery.tapValue (fun x ->
            stateObservers |> Seq.iter (fun f -> f x)))