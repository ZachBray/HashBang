namespace TypeInferred.HashBang.Html

open FunScript

type OnValid<'TValue> = 'TValue -> unit
type OnInvalid = FormInvalidReason -> unit

[<JS>]
type FormQuery<'TValue, 'TElement> = FormQuery of (OnValid<'TValue> -> OnInvalid -> 'TElement[])

[<JS; RequireQualifiedAccess>]
module FormQuery =

    let puree x = FormQuery(fun onValid _ -> onValid x; [||])

    let delay f = FormQuery(fun onValid onInvalid ->
        let (FormQuery g) = f()
        g onValid onInvalid)

    let merge (FormQuery fs) (FormQuery xs) = 
        FormQuery(fun onValid onInvalid ->
            let latestF = ref (Invalid Unset)
            let latestX = ref (Invalid Unset)
            let triggerUpdate() =
                match !latestF, !latestX with
                | Valid f, Valid x -> onValid (f x)
                | Invalid reason, _ | _, Invalid reason -> onInvalid reason
            let fsElements =
                fs (fun f -> latestF := Valid f; triggerUpdate()) 
                   (fun reason -> latestF := Invalid reason; triggerUpdate())
            let xsElements =
                xs (fun x -> latestX := Valid x; triggerUpdate())
                   (fun reason -> latestX := Invalid reason; triggerUpdate())
            Array.append fsElements xsElements)

    let fromFunctions f = FormQuery f

    let mapElements f (FormQuery xs) = 
        FormQuery(fun onValid onInvalid -> 
            f(xs onValid onInvalid))

    let mapValue f (FormQuery xs) = 
        FormQuery(fun onValid onInvalid -> 
            let processResult x =
                match f x with
                | Valid y -> onValid y
                | Invalid reason -> onInvalid reason
            xs (fun x -> processResult(Valid x))
               (fun reason -> processResult(Invalid reason)))

    let tapValue f (FormQuery xs) = 
        FormQuery(fun onValid onInvalid -> 
            let processResult x =
                f x
                match x with
                | Valid y -> onValid y
                | Invalid reason -> onInvalid reason
            xs (fun x -> processResult(Valid x))
               (fun reason -> processResult(Invalid reason)))

    let filter f (FormQuery xs) = 
        FormQuery(fun onValid onInvalid -> 
            let processResult x =
                match f x with
                | Valid y -> onValid y
                | Invalid reason -> onInvalid reason
            xs (fun x -> processResult x)
               (fun reason -> onInvalid reason))

    let filterAsync f (FormQuery xs) = 
        FormQuery(fun onValid onInvalid -> 
            let processResult x =
                async {
                    let! y = f x
                    match y with
                    | Valid z -> onValid z
                    | Invalid reason -> onInvalid reason
                } |> Async.StartImmediate
            xs (fun x -> processResult x)
               (fun reason -> onInvalid reason))

[<JS; AutoOpen>]
module FormQueryOperators =
    let (<*>) fs xs = FormQuery.merge fs xs