[<ReflectedDefinition>]
module TypeInferred.HashBang.Runtime

type PrimitiveType =
    | BoolType
    | IntType
    | StringType
    | UIntType
    //| HexType
    //| OctalType
    | FloatType
    | DecimalType
    | CharType

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
            | _ -> failwith  ("Unsupported primitive type: " + typeId)

type UrlPart =
    | FixedPart of string
    | VariablePart of string * PrimitiveType

    static member Create(template : string) =
        match template.Split [|':'|] |> Array.toList with
        | [name; typeId] -> VariablePart(name, PrimitiveType.Create typeId)
        | [name] -> FixedPart name
        | _ -> failwith ("Unsupported segment format: " + template)

type ContentType = 
    | ContentType of string
    member x.Mime = 
        let (ContentType mime) = x
        mime