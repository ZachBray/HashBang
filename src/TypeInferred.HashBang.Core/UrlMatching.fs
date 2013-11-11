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

    static member Create(typeId : string) =
            match typeId with
            | "bool" -> BoolType
            | "int" -> IntType
            | "string" -> StringType
            | "uint" -> UIntType
            //| "hex" -> HexType
            | "float" -> FloatType
            | "decimal" -> DecimalType
            | "char" -> CharType
            | _ -> failwithf  "Unsupported primitive type: '%s'." typeId

type UrlPart with

    static member Create(template : string) =
            match template.Split [|':'|] with
            | [| name; typeId |] -> VariablePart(name, PrimitiveType.Create typeId)
            | [| name |] -> FixedPart name
            | _ -> failwithf "Unsupported segment format: '%s'." template