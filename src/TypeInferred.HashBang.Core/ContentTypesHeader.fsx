[<ReflectedDefinition>]
module TypeInferred.HashBang.Runtime

type PrimitiveType =
    | BoolType
    | IntType
    | StringType
    | UIntType
    | HexType
    //| OctalType
    | FloatType
    | DecimalType
    | CharType

type UrlPart =
    | FixedPart of string
    | VariablePart of string * PrimitiveType

type ContentType = 
    | ContentType of string
    member x.Mime = 
        let (ContentType mime) = x
        mime