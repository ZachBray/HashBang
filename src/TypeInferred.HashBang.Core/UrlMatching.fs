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
        | HexType -> failwith "Hex numbers are not yet supported."
        //| OctalType
        | FloatType -> typeof<float>
        | DecimalType -> typeof<decimal>
        | CharType -> typeof<char>

type UrlPart with

    static member From(template : string) =
        let inside = template.Trim[|'{'; '}'|]
        if inside.Length = template.Length then FixedPart(template.Replace("%%", "%"))
        elif inside.Length = template.Length - 2 then
            match inside.Split [|':'|] with
            | [| name; placeholder |] ->
                let primitiveType =
                    match placeholder with
                    | "%b" -> BoolType
                    | "%d" | "%i" -> IntType
                    | "%s" -> StringType
                    | "%u" -> UIntType
                    | "%x" | "%X" -> HexType
                    //| "%o" -> OctalType |> VariablePart
                    | "%e" | "%E" | "%f" | "%F" | "%g" | "%G" -> FloatType
                    | "%M" -> DecimalType
                    | "%c" -> CharType
                    | _ -> failwithf  "Unsupported placeholder: '%s'." placeholder
                VariablePart(name, primitiveType)
            | _ -> failwithf "Unsupported section format: '%s'." template
        else failwithf "Unsupported section format: '%s'." template

    member part.TryParse(value : string) =
        let inline apply tryParse =
            let x = ref Unchecked.defaultof<_>
            let didParse : bool = tryParse(value, x)
            if didParse then SpecifiedArgument(box x)
            else InvalidArgument
        match part with
        | FixedPart v -> 
            if v = value then NoArgument
            else InvalidArgument
        | VariablePart(_, t) ->
            match t with
            | BoolType -> 
                apply (fun (value, x) -> 
                    Boolean.TryParse(value, x))
            | IntType -> apply Int32.TryParse
            | StringType -> SpecifiedArgument(box value)
            | UIntType -> apply UInt32.TryParse
            | HexType -> 
                apply (fun (value, x) ->
                    UInt32.TryParse(value, NumberStyles.HexNumber, CultureInfo.InvariantCulture, x))
            | FloatType -> apply Single.TryParse
            | DecimalType -> apply Decimal.TryParse
            | CharType -> apply (fun (value, x) -> Char.TryParse(value, x))