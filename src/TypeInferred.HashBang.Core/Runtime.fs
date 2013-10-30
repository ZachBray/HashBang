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

type ApplicationContentType =
    | AtomXml
    | JavaScript
    | Json
    | OctetStream
    | Xml
    member t.Mime =
        match t with
        | AtomXml -> "atom+xml"
        | JavaScript -> "javascript"
        | Json -> "json"
        | OctetStream -> "octet-stream"
        | Xml -> "xml"

type TextContentType =
    | Css
    | Html
    | Plain
    member t.Mime =
        match t with
        | Css -> "css"
        | Html -> "html"
        | Plain -> "plain"

type ContentType =
    | Application of ApplicationContentType
    | Text of TextContentType
    member t.Mime =
        match t with
        | Application subType -> "application/" + subType.Mime
        | Text subType -> "text/" + subType.Mime

