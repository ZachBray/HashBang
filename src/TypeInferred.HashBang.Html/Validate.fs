[<FunScript.JS; RequireQualifiedAccess>]
module TypeInferred.HashBang.Html.Validate

open System
open FunScript

let isNotEmpty f = 
    f |> FormQuery.filter (fun text -> 
        if String.IsNullOrEmpty text then Invalid Unset
        else Valid text)

let isAtLeast n f =
    f |> FormQuery.filter (fun text -> 
        if String.IsNullOrEmpty text then Invalid Unset
        elif text.Length < n then Invalid(Error (sprintf "Must be at least %i characters." n))
        else Valid text)

let isA tryParse msg f =
    f |> FormQuery.filter (fun text ->
        if String.IsNullOrEmpty text then Invalid Unset
        else 
            match tryParse text with
            | Some x -> Valid x
            | None -> Invalid(Error msg))

let isInt f = isA Int32.TryParseJS "Expected an integer." f

let isFloat f = isA Double.TryParseJS "Expected a number." f

let isBool f = isA Boolean.TryParseJS "Expected true or false." f