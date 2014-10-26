namespace TypeInferred.HashBang.Html

open System
open FunScript
open FunScript.TypeScript

[<JS>]
module internal ResizeArray =
    let iter f (xs : ResizeArray<_>) =
        for i = 0 to xs.Count - 1 do
            f xs.[i]

type OnValid<'TValue> = 'TValue -> unit
type OnInvalid = FormInvalidReason -> unit

[<JS>]
type FormStatus = Active | Disabled

[<JS>]
type FormContext<'TValue> =
    {
        OnValidated : 'TValue -> unit
        OnInvalidated : FormInvalidReason -> unit
        OnStatusChanged : FormStatus -> unit
        SubscribeFormStatus : (FormStatus -> unit) -> IDisposable
        SubscribeLocalValue : ('TValue FormQueryResult -> unit) -> IDisposable
    }

[<JS>]
module FormContext = 
    let fromFunctions (context : FormContext<'b>) onValidated onInvalidated =
        let observers = ResizeArray()
        let latestValue = ref (Invalid Unset)
        {
            OnValidated = fun (x : 'a) -> 
                latestValue := Valid x
                observers |> ResizeArray.iter (fun onValue -> onValue !latestValue)
                onValidated x
            OnInvalidated = fun reason -> 
                latestValue := Invalid reason
                observers |> ResizeArray.iter (fun onValue -> onValue !latestValue)
                onInvalidated reason
            OnStatusChanged = context.OnStatusChanged
            SubscribeFormStatus = context.SubscribeFormStatus
            SubscribeLocalValue = fun onValue ->
                observers.Add onValue
                onValue !latestValue
                Disposable.by (fun () -> observers.Remove onValue |> ignore)
        }

[<JS>]
type FormQuery<'TValue, 'TElement> = FormQuery of (FormContext<'TValue> -> 'TElement[])

[<JS; RequireQualifiedAccess>]
module FormQuery =

    let puree x = FormQuery(fun context -> context.OnValidated x; [||])

    let delay f = FormQuery(fun context ->
        let (FormQuery g) = f()
        g context)

    let merge (FormQuery fs : FormQuery<'a -> 'b, _>) (FormQuery xs) = 
        FormQuery(fun context ->
            let latestF = ref (Invalid Unset)
            let latestX = ref (Invalid Unset)
            let triggerUpdate() =
                match !latestF, !latestX with
                | Valid f, Valid x -> context.OnValidated (f x)
                | Invalid reason, _ | _, Invalid reason ->
                    context.OnInvalidated reason
            let fsElements =
                FormContext.fromFunctions 
                    context
                    (fun f -> latestF := Valid f; triggerUpdate())
                    (fun reason -> latestF := Invalid reason; triggerUpdate())
                |> fs
            let xsElements =
                FormContext.fromFunctions 
                    context
                    (fun x -> latestX := Valid x; triggerUpdate())
                    (fun reason -> latestX := Invalid reason; triggerUpdate())
                |> xs
            Array.append fsElements xsElements)

    let fromContext f = FormQuery f

    let mapElements f (FormQuery xs) = FormQuery(fun context -> f (xs context) context)

    let mapValue f (FormQuery xs) = 
        FormQuery(fun context -> 
            let processResult x =
                match f x with
                | Valid y -> context.OnValidated y
                | Invalid reason -> context.OnInvalidated reason
            FormContext.fromFunctions
                context
                (fun x -> processResult(Valid x))
                (fun reason -> processResult(Invalid reason))
            |> xs)

    let tapValue f xs = mapValue (fun x -> f x; x) xs

    let filter f (FormQuery xs) = 
        FormQuery(fun context -> 
            let processResult x =
                match f x with
                | Valid y -> context.OnValidated y
                | Invalid reason -> context.OnInvalidated reason
            FormContext.fromFunctions
                context
                processResult
                context.OnInvalidated
            |> xs)

    let filterAsync f (FormQuery xs) = 
        FormQuery(fun context -> 
            let processResult x =
                // TODO: cancel last async if still in progress!
                async {
                    let! y = f x
                    match y with
                    | Valid z -> context.OnValidated z
                    | Invalid reason -> context.OnInvalidated reason
                } |> Async.StartImmediate
            FormContext.fromFunctions
                context
                processResult
                context.OnInvalidated
            |> xs)

    let run (FormQuery xs) =
        let stateObservers = ResizeArray()
        let latestState = ref Active
        let valueObservers = ResizeArray()
        let latestValue = ref (Invalid Unset)
        xs {
            OnValidated = fun (value : 'a) ->
                latestValue := Valid value
                valueObservers |> ResizeArray.iter (fun onValue -> onValue !latestValue)
            OnInvalidated = fun reason ->
                latestValue := Invalid reason
                valueObservers |> ResizeArray.iter (fun onValue -> onValue !latestValue)
            OnStatusChanged = fun state ->
                latestState := state
                stateObservers |> ResizeArray.iter (fun onState -> onState !latestState)
            SubscribeFormStatus = fun onState ->
                stateObservers.Add onState
                onState !latestState
                Disposable.by (fun () -> stateObservers.Remove onState |> ignore)
            SubscribeLocalValue = fun onValue ->
                valueObservers.Add onValue
                onValue !latestValue
                Disposable.by (fun () -> valueObservers.Remove onValue |> ignore)
        }
        |> Array.toList
        |> List.map (fun x -> x :> IHtmlTag)
        |> form []
        

[<JS; AutoOpen>]
module FormQueryOperators =
    let (<*>) fs xs = FormQuery.merge fs xs