module TypeInferred.HashBang.Owin.FunScriptInterop

open System
open FunScript
open TypeInferred.HashBang.Routing.Conversion

[<JS>]
module Replacements =

    let toNullable = function
        | Some x -> Nullable(x)
        | None -> Nullable()

    let tryParseInt32 x = Int32.TryParseJS x |> toNullable
    let tryParseUInt32 x = UInt32.TryParseJS x |> toNullable
    let tryParseDouble x = Double.TryParseJS x |> toNullable
    let tryParseBoolean x = Boolean.TryParseJS x |> toNullable
    let tryParseChar (x:string) = Nullable(x.[0])

open Replacements

let components =
    [
        ExpressionReplacer.create <@ Int32.TryConvert @> <@ tryParseInt32 @>
        ExpressionReplacer.create <@ UInt32.TryConvert @> <@ tryParseUInt32 @>
        ExpressionReplacer.create <@ Double.TryConvert @> <@ tryParseDouble @>
        ExpressionReplacer.create <@ Boolean.TryConvert @> <@ tryParseBoolean @>
        ExpressionReplacer.create <@ Char.TryConvert @> <@ tryParseChar @>
    ]