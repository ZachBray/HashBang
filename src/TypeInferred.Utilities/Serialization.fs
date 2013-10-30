[<AutoOpen>]
[<ReflectedDefinition>]
module SerializationExtensions

open System
open System.IO
open System.Text
open System.Reflection
open Microsoft.FSharp.Reflection

let accessFlags = 
    BindingFlags.Public ||| BindingFlags.NonPublic

let (|RecordType|_|) t =
    if FSharpType.IsRecord t then
        FSharpType.GetRecordFields(t, accessFlags)
        |> Some
    else None

let (|UnionType|_|) (t : Type) = 
    if FSharpType.IsUnion t then 
        FSharpType.GetUnionCases t
        |> Some
    else None

let (|TupleType|_|) t =
    if FSharpType.IsTuple t then 
        FSharpType.GetTupleElements t
        |> Some
    else None

let (|ArrayType|_|) (t : Type) = 
    if t.IsArray (*&& t.HasElementType*) then
        Some(t.GetElementType())
    else None

let isType<'a> (t : Type) =
    if t.FullName = typeof<'a>.FullName then Some unbox<'a>
    else None

let (|StringType|_|) t = isType<string> t
let (|Int32Type|_|) t = isType<int> t
let (|Int64Type|_|) t = isType<int64> t
let (|UInt32Type|_|) t = isType<uint32> t
let (|UInt64Type|_|) t = isType<uint64> t
let (|FloatType|_|) t = isType<float> t
let (|DecimalType|_|) t = isType<decimal> t
let (|BoolType|_|) t = isType<bool> t


type JsonReader(jsonStr : string) =
    let mutable position = 0

    let tryPeek() = 
        if position < jsonStr.Length then
            Some jsonStr.[position]
        else None

    let tryConsume() = 
        if position < jsonStr.Length then
            let char = jsonStr.[position]
            position <- position + 1
            Some char
        else None

    let consume() =
        position <- position + 1
    
    let rec consumeWhiteSpaceUpToIncl char =
        match tryConsume() with
        | None -> failwith "Out of range."
        | Some c ->
            match c with
            | '\n' | '\r' | '\t' | ' ' -> consumeWhiteSpaceUpToIncl char
            | _ when c = char -> ()
            | _ -> failwith("Unexpected character: " + c.ToString())

    let rec consumeWhiteSpace() =
        match tryPeek() with
        | None -> failwith "Out of range."
        | Some c ->
            match c with
            | '\n' | '\r' | '\t' | ' ' -> consume(); consumeWhiteSpace()
            | _ -> () 

    let readUpToExcl charSet =
        let rec readUpToExcl count =
            let nextC = tryPeek()
            match nextC with
            | Some c when charSet |> Set.contains c -> count
            | Some _ -> consume(); readUpToExcl (count + 1)
            | _ -> failwith "Out of range."
        let startPosition = position
        let count = readUpToExcl 0
        jsonStr.Substring(startPosition, count)

    let separators =
        let chars = [|','; '}'; ']'; ' '; '\t'; '\r'; '\n'|]
        chars |> Set.ofArray

    let quoteMarks =
        let chars = [|'"'|]
        chars |> Set.ofArray

    member __.Peek() = 
        match tryPeek() with
        | Some c -> c
        | None -> failwith "Out of range."

    member __.MovePastWhiteSpace() =
        consumeWhiteSpace()

    member __.MovePast char =
        consumeWhiteSpaceUpToIncl char

    member __.ReadUpToQuoteMark() =
        readUpToExcl quoteMarks
        
    member __.ReadUpToSeparatorInclWhiteSpace() =
        readUpToExcl separators

module JsonReaderHelpers =
    let readJsonObject (positions : Map<string, int>) (readers : _ Lazy []) (jr : JsonReader) =
        jr.MovePast '{'
        let args = Array.zeroCreate readers.Length
        for i = 0 to args.Length - 1 do
            if i <> 0 then
                jr.MovePast ','
            jr.MovePast '"'
            let name = jr.ReadUpToQuoteMark()
            let position = positions.[name]
            let readArg = readers.[position]
            jr.MovePast '"'
            jr.MovePast ':'
            let arg = readArg.Value jr
            args.[position] <- arg
        jr.MovePast '}'
        args

    let readValue parse (jr : JsonReader) =
        jr.MovePastWhiteSpace()
        let stringValue = jr.ReadUpToSeparatorInclWhiteSpace()
        parse stringValue |> box

open JsonReaderHelpers

let precomputeDeserializerAux (precomputeDeserializer : _ -> _ Lazy) t : JsonReader -> obj =
    match t with
    | RecordType pis ->
        let cons = FSharpValue.PreComputeRecordConstructor(t, accessFlags)
        let positions = pis |> Array.mapi (fun i pi -> pi.Name, i) |> Map.ofArray
        let readers = pis |> Array.map (fun pi -> precomputeDeserializer pi.PropertyType)
        fun (jr : JsonReader) ->
            let args = readJsonObject positions readers jr
            cons args
    | UnionType ucis ->
        let uciInfos = ucis |> Array.map (fun uci -> 
            let cons = FSharpValue.PreComputeUnionConstructor(uci, accessFlags)
            let positions = uci.GetFields() |> Array.mapi (fun i pi -> pi.Name, i) |> Map.ofArray
            let readers = uci.GetFields() |> Array.map (fun pi -> precomputeDeserializer pi.PropertyType)
            cons, positions, readers)
        let tagPositions = ucis |> Array.mapi (fun i uci -> uci.Name, i) |> Map.ofArray
        fun (jr : JsonReader) ->
            jr.MovePast '{'
            jr.MovePast '"'
            let tagName = jr.ReadUpToQuoteMark()
            jr.MovePast '"'
            jr.MovePast ':'
            let tagPosition = tagPositions.[tagName]
            let cons, positions, readers = uciInfos.[tagPosition]
            let args = readJsonObject positions readers jr
            jr.MovePast '}'
            cons args
    | ArrayType elementType ->
        let readElement = precomputeDeserializer elementType
        fun (jr : JsonReader) ->
            jr.MovePast '['
            jr.MovePastWhiteSpace()
            let rec buildElements acc =
                match jr.Peek() with
                | ']' -> acc
                | _ ->
                    let element = readElement.Value jr
                    jr.MovePastWhiteSpace()
                    match jr.Peek() with
                    | ',' -> jr.MovePast ','
                    | _ -> ()
                    buildElements (element :: acc)
            let xs = buildElements [] |> List.toArray
            let ys = Array.CreateInstance(elementType, xs.Length)
            for i = 0 to xs.Length - 1 do
                ys.SetValue(xs.[xs.Length - 1 - i], i)
            jr.MovePast ']'
            box ys
    | TupleType ts ->
        let cons = FSharpValue.PreComputeTupleConstructor t
        let positions = ts |> Array.mapi (fun i _ -> "Item" + i.ToString(), i) |> Map.ofArray
        let readers = ts |> Array.map precomputeDeserializer
        fun (jr : JsonReader) ->
            let args = readJsonObject positions readers jr
            cons args
    | StringType extract ->
        fun (jr : JsonReader) ->
            jr.MovePast '"'
            let value = jr.ReadUpToQuoteMark()
            jr.MovePast '"'
            box value
    | Int32Type _ -> fun jr -> readValue Int32.Parse jr
    | Int64Type _ -> fun jr -> readValue Int64.Parse jr
    | UInt32Type _ -> fun jr -> readValue UInt32.Parse jr
    | UInt64Type _ -> fun jr -> readValue Int64.Parse jr
    | FloatType _ -> fun jr -> readValue Double.Parse jr
    | DecimalType _ -> fun jr -> readValue Decimal.Parse jr
    | BoolType _ -> fun jr -> readValue Boolean.Parse jr
    | _ -> failwith ("Cannot serialize type: " + t.Name)

type StringWriter with
    member sw.Append(value : obj) =
        sw.Write value; sw

let precomputeSerializerAux (precomputeSerializer : _ -> _ Lazy) t : obj -> StringWriter -> unit =
    match t with
    | RecordType pis ->
        let propWriters = pis |> Array.map (fun pi -> 
            let propertyReader = FSharpValue.PreComputeRecordFieldReader pi
            let propertySerializer = precomputeSerializer pi.PropertyType
            let name = pi.Name
            name, fun obj sb ->
                let propertyObject = propertyReader obj
                propertySerializer.Value propertyObject sb)
        fun obj (sb : StringWriter) ->
            sb.Append "{" |> ignore
            for i = 0 to propWriters.Length - 1 do
                if i <> 0 then
                    sb.Append(",") |> ignore
                let name, writer = propWriters.[i]
                sb.Append('"').Append(name).Append('"').Append(":") |> ignore
                writer obj sb
            sb.Append "}" |> ignore
    | UnionType ucis ->
        let readTag = FSharpValue.PreComputeUnionTagReader(t, accessFlags)
        let caseReaders = ucis |> Array.map (fun uci -> 
            let readCase = FSharpValue.PreComputeUnionReader(uci, accessFlags)
            let fieldNames = uci.GetFields() |> Array.map (fun pi -> pi.Name)
            let serializers = uci.GetFields() |> Array.map (fun pi -> precomputeSerializer pi.PropertyType)
            fieldNames, readCase, serializers)
        let tagNames = ucis |> Array.map (fun uci -> uci.Name)
        fun obj (sb : StringWriter) ->
            let tag = readTag obj
            let name = tagNames.[tag]
            sb.Append("{").Append('"').Append(name).Append('"').Append(":{") |> ignore
            let fieldNames, readCase, serializers = caseReaders.[tag]
            let fieldValues = readCase obj
            for i = 0 to fieldValues.Length - 1 do
                if i <> 0 then
                    sb.Append(",") |> ignore
                let fieldValue = fieldValues.[i]
                let fieldName = fieldNames.[i]
                let serialize = serializers.[i]
                sb.Append('"').Append(fieldName).Append('"').Append(":") |> ignore
                serialize.Value fieldValue sb
            sb.Append("}}") |> ignore
    | ArrayType elementType ->
        let serializeElement = precomputeSerializer elementType
        fun obj (sb : StringWriter) ->
            let xs = unbox<Array> obj
            sb.Append("[") |> ignore
            for i = 0 to xs.Length - 1 do
                if i <> 0 then
                    sb.Append(",") |> ignore
                serializeElement.Value (xs.GetValue i) sb
            sb.Append("]") |> ignore
    | TupleType ts ->
        let readTuple = FSharpValue.PreComputeTupleReader t
        let elementSerializers = ts |> Array.map precomputeSerializer
        fun obj (sb : StringWriter) ->
            let elements = readTuple obj
            sb.Append "{" |> ignore
            for i = 0 to elements.Length - 1 do
                if i <> 0 then
                    sb.Append(",") |> ignore
                let element = elements.[i]
                let serialize = elementSerializers.[i]
                sb.Append('"').Append("Item").Append(i).Append('"').Append(":") |> ignore
                serialize.Value element sb
            sb.Append "}" |> ignore
    | StringType extract ->
        fun obj (sb : StringWriter) ->
            let x = extract obj
            sb.Append('"').Append(x).Append('"') |> ignore
    | Int32Type extract ->
        fun obj (sb : StringWriter) ->
            let x = extract obj
            sb.Append(x) |> ignore
    | Int64Type extract ->
        fun obj (sb : StringWriter) ->
            let x = extract obj
            sb.Append(x) |> ignore
    | UInt32Type extract ->
        fun obj (sb : StringWriter) ->
            let x = extract obj
            sb.Append(x) |> ignore
    | UInt64Type extract ->
        fun obj (sb : StringWriter) ->
            let x = extract obj
            sb.Append(x) |> ignore
    | FloatType extract ->
        fun obj (sb : StringWriter) ->
            let x = extract obj
            sb.Append(x) |> ignore
    | DecimalType extract ->
        fun obj (sb : StringWriter) ->
            let x = extract obj
            sb.Append(x) |> ignore
    | BoolType extract ->
        fun obj (sb : StringWriter) ->
            // NOTE: needs to be in lower case
            let value =
                match extract obj with
                | true -> "true"
                | false -> "false"
            sb.Append(value) |> ignore
    | _ -> failwith ("Cannot serialize type: " + t.Name)

let precomputeSerializer t =
    //TODO: Remove memoization for javascript version? Or implement lazy and conc.dict.?
    memoizeRecLazy precomputeSerializerAux t

let precomputeTypeToJson t =
    let serialize = precomputeSerializer t
    fun (obj : obj) ->
        use sb = new StringWriter()
        serialize obj sb
        sb.ToString()

let precomputeToJson<'a>() =
    let serialize = precomputeSerializer typeof<'a>
    fun (obj : 'a) ->
        use sb = new StringWriter()
        serialize obj sb
        sb.ToString()

let precomputeDeserializer t =
    //TODO: Remove memoization for javascript version? Or implement lazy and conc.dict.?
    memoizeRecLazy precomputeDeserializerAux t

let precomputeTypeFromJson t =
    let deserialize = precomputeDeserializer t
    fun (json : string) ->
        let reader = JsonReader(json)
        deserialize reader

let precomputeFromJson<'a>() =
    let deserialize = precomputeDeserializer typeof<'a>
    fun (json : string) ->
        let reader = JsonReader(json)
        deserialize reader :?> 'a

