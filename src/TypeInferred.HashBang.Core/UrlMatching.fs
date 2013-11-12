[<AutoOpen>]
module TypeInferred.HashBang.UrlMatching

open System

open TypeInferred.HashBang.Runtime
open System.Globalization

type UrlArgument =
    | NoArgument
    | SpecifiedArgument of obj
    | InvalidArgument

type PrimitiveType with

    member pt.RuntimeType =
        match pt with
        | BoolType -> typeof<bool>
        | IntType -> typeof<int>
        | StringType -> typeof<string>
        | UIntType -> typeof<uint32>
        //| HexType -> failwith "Hex numbers are not yet supported."
        //| OctalType
        | FloatType -> typeof<float>
        | DecimalType -> typeof<decimal>
        | CharType -> typeof<char>