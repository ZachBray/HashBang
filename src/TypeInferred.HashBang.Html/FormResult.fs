namespace TypeInferred.HashBang.Html

open FunScript

[<JS>]
type FormInvalidReason =
    | Unset
    | Error of string

[<JS>]
type 'TValue FormQueryResult = 
    | Valid of 'TValue 
    | Invalid of FormInvalidReason

[<JS; RequireQualifiedAccess>]
module FormQueryResult =
    let map f = function
        | Valid x -> Valid(f x)
        | Invalid reason -> Invalid reason
