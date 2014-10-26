[<FunScript.JS; RequireQualifiedAccess>]
module TypeInferred.HashBang.Html.Validate

open System
open FunScript

let by f = f |> FormQuery.filter

let byAsync f = f |> FormQuery.filterAsync

let checkThat g msg f =
    f |> FormQuery.filter (fun x -> if g x then Valid x else Invalid(FieldError msg))

let checkThatAsync g msg f =
    f |> FormQuery.filterAsync (fun x -> 
        async {
            let! isValid = g x
            if isValid then return Valid x else return Invalid(FieldError msg)
        })

let isNotEmpty f = 
    f |> FormQuery.filter (fun text -> 
        if String.IsNullOrEmpty text then Invalid Unset
        else Valid text)

let isAtLeast n f =
    f |> isNotEmpty 
    |> checkThat (fun text -> text.Length >= n) (sprintf "Must be at least %i characters." n)

let isA tryParse msg f =
    f |> FormQuery.filter (fun text ->
        if String.IsNullOrEmpty text then Invalid Unset
        else 
            match tryParse text with
            | Some x -> Valid x
            | None -> Invalid(FieldError msg))

let isInt f = isA Int32.TryParseJS "Expected an integer." f

let isFloat f = isA Double.TryParseJS "Expected a number." f

let isBool f = isA Boolean.TryParseJS "Expected true or false." f