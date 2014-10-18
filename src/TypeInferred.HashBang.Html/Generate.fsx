#r "System.Xml"
#r "System.Xml.Linq"
#r @"..\..\temp\HtmlAgilityPack.dll"
#r @"..\..\packages\FunScript\lib\net40\FunScript.dll"
#r @"..\..\packages\FunScript\lib\net40\FunScript.Interop.dll"
#r @"..\..\packages\FunScript.TypeScript.Binding.lib\lib\net40\FunScript.TypeScript.Binding.lib.dll"
#r @"..\..\packages\FunScript.TypeScript.Binding.jquery\lib\net40\FunScript.TypeScript.Binding.jquery.dll"

open System
open System.IO
open System.Net
open HtmlAgilityPack
open FunScript.TypeScript

let downloadAsync (url : string) =
    async {
        let req = WebRequest.Create url
        let! resp = req.AsyncGetResponse()
        use stream = resp.GetResponseStream()
        use reader = new StreamReader(stream)
        return! Async.AwaitTask(reader.ReadToEndAsync())
    }

let rec traverse f (x : HtmlNode) =
    let xs = [|for x in x.ChildNodes -> x|]
    xs |> Array.iter (traverse f)
    f x

let choose f x =
    let ys = ResizeArray()
    x |> traverse (fun z ->
        match f z with
        | None -> ()
        | Some y -> ys.Add y)
    ys.ToArray()

let tryGetAttribute (x : string) (y : HtmlNode) =
    if y.Attributes.Contains x then Some y.Attributes.[x].Value
    else None

let (?) y x = tryGetAttribute x y

let findTagPages() = 
    async {
        let! text = downloadAsync "http://www.w3schools.com/tags/"
        let doc = HtmlDocument()
        doc.LoadHtml text
        return 
            doc.DocumentNode |> choose (fun n ->
                match n?href with
                | Some v when v.StartsWith "tag_" ->
                    Some v
                | Some _ | None -> None)
            |> Array.filter (function
                | "tag_doctype.asp"
                | "tag_comment.asp"
                | "tag_hn.asp" -> false
                | _ -> true)
            |> Seq.distinct
            |> Seq.toArray
            |> Array.map (fun x ->
                let tag = x.Split([|'_'; '.'|]).[1]
                tag,
                "http://www.w3schools.com/tags/" + x,
                "http://www.w3.org/TR/html-markup/" + tag + ".html")
    }

/// See: http://www.w3.org/TR/html-markup/syntax.html and search for "void element"
let voidElements = Set ["area"; "base"; "br"; "col"; "command"; "embed"; "hr"; "img"; "input"; "keygen"; "link"; "meta"; "param"; "source"; "track"; "wbr"]

let isVoid (tag : string) = voidElements.Contains(tag.ToLowerInvariant())

type ValueType =
    | Empty
    | Case of string
    | StringVariable of string
    | NumberVariable of string
    | PixelsVariable of string

let findAttributes tag root =
    root |> choose (fun n ->
        match n.Name with
        | "tr" ->
            let children = n.ChildNodes |> Seq.filter (fun n -> n.Name <> "#text") |> Seq.toArray
            if children.Length = 3 && 
               children |> Array.forall (fun n -> n.Name = "td") &&
               not(children.[2].InnerText.Contains("Not supported in HTML")) then
                let nameNode, valuesNode, descriptionNode = children.[0], children.[1], children.[2]
                let name = 
                    let text = nameNode.InnerText
                    if text.EndsWith "New" then text.Substring(0, text.Length - "New".Length)
                    else text
                if name.StartsWith "on" then None
                else
                    let values =
                        valuesNode.InnerHtml.Split([|"<br>"|], StringSplitOptions.RemoveEmptyEntries)
                        |> Array.map (fun str -> str.Trim())
                        |> Array.map (fun str ->
                            let clean (str : string) =  
                                str.Replace("<i>", "").Replace("</i>", "")
                                   .Replace("<em>", "").Replace("</em>", "")
                                   .Replace("\n", "").Replace("\r", "")
                                   .Replace("\t", "")
                                   .Replace("&nbsp;", "")
                            if str = name then Empty
                            elif (clean str).ToLower().Contains "pixels" then
                                PixelsVariable(clean str)
                            elif Int32.TryParse(clean str, ref 0) then
                                NumberVariable "number"
                            elif str.Contains "<i>" || str.Contains "<em>" then
                                StringVariable(clean str)
                            elif str.StartsWith "#" then
                                StringVariable(clean (str.Substring 1))
                            else Case(clean str))
                    let description = 
                        descriptionNode.InnerText.Split([|'\r'; '\n'|])
                        |> Seq.map (fun str -> str.Trim())
                        |> String.concat " "
                        |> fun str ->
                            str.Replace("&lt;","<")
                               .Replace("&gt;",">")
                               .Replace("&quot;","\"")
                               .Replace("&nbsp;"," ")
                    Some(name, values, description)
            else None
        | _ -> None)

let findElementType(text:string) =
    let prefix = "class=\"idl\""
    let i = text.IndexOf prefix + prefix.Length
    let section = text.Substring i
    let j = section.IndexOf "HTML"
    let wordSection = section.Substring j
    let n = wordSection.IndexOf "<"
    wordSection.Substring(0, n)

let findTagAttributes tag page w3page =
    async {
        let! text = downloadAsync page
        let doc = HtmlDocument()
        doc.LoadHtml text
        let attributes = findAttributes tag doc.DocumentNode
        let isVoid = isVoid tag
        try
            let! w3text = downloadAsync w3page
            let typeScriptType = findElementType w3text
            return tag, isVoid, attributes, typeScriptType
        with ex -> 
            return tag, isVoid, attributes, "HTMLElement"
    }

let findTags() =
    async {
        let! pages = findTagPages()
        return! 
            pages |> Array.map (fun (tag, page, w3page) ->
                findTagAttributes tag page w3page)
            |> Async.Parallel
    }

let fsharpKeywords =
    Set [|  "async"; "method"; "atomic"; "mixin"; "break"; "namespace"; "checked";
            "object"; "component"; "process"; "const"; "property"; "constraint";
            "protected"; "constructor"; "public"; "continue"; "pure"; "decimal";
            "readonly"; "eager"; "return"; "enum"; "sealed"; "event"; "switch";
            "external"; "virtual"; "fixed"; "void"; "functor"; "volatile"; "include";
            "where"; "abstract"; "lsl"; "and"; "lsr"; "as"; "lxor"; "assert"; "match";
            "member"; "asr"; "mod"; "begin"; "module"; "class"; "mutable"; "namespace";
            "default"; "new"; "delegate"; "null"; "do"; "of"; "done"; "open";
            "downcast"; "or"; "downto"; "override"; "else"; "rec"; "end"; "sig";
            "exception"; "static"; "false"; "struct"; "finally"; "then"; "for"; "to";
            "fun"; "true"; "function"; "try"; "if"; "type"; "in"; "val"; "inherit";
            "when"; "inline"; "upcast"; "interface"; "while"; "land"; "with"; "lor";
            "measure"; "atomic"; "break"; "checked"; "component"; "const";
            "constraint"; "constructor"; "continue"; 
            "eager"; "event"; "external"; "fixed"; "functor"; "include";
            "method"; "mixin"; "object"; "parallel"; "process"; "protected"; "pure";
            "sealed"; "tailcall"; "trait"; "virtual"; "volatile"; "asr"; "land"; "lor";
            "lsl"; "lsr"; "lxor"; "mod"; "sig"; "int"; "sbyte"; "byte"; "uint";
            "uint32"; "int32"; "int64"; "uint64"; "int16"; "uint16"; "float";
            "decimal"; "float32"; "bool"; "obj"; "unit"; "global"; "recursive"; 
            "use"; "let"; "do"; "yield"; "lazy"; "constructor"; "base"|]

let clean (str : string) =
    str.TrimStart([|'_'|])
       .Replace(" ", "_")
       .Replace(":", "_")
       .Replace("&nbsp;", "")
       .Replace("/", "_")
       .Replace("\\", "_")
       .Replace("-", "_")
       .Replace(".", "_")
       .Replace("&quot;", "")
       .Replace("*", "WildCard")

let toPascalCase name =
    name |> clean |> String.mapi (fun i c ->
        match i, Char.IsUpper c with
        | 0, false -> Char.ToUpper c
        | _ -> c)
    |> function
        | "" -> "Empty"
        | "Map" -> "MapElement"
        | x -> x

let toCamelCase name =
    name |> clean |> String.mapi (fun i c ->
        match i, Char.IsUpper c with
        | 0, true -> Char.ToLower c
        | _ -> c)
    |> function
       | x when fsharpKeywords.Contains x -> sprintf "``%s``" x
       | x -> x  

let generatedTypesByValues = ref Map.empty
let generatedTypes = ref Map.empty

let rec getGeneratedName i holderT name values =
    match !generatedTypesByValues |> Map.tryFind values with
    | Some t -> t, false
    | None ->
        let t =
            if i = 0 then toPascalCase name
            elif i = 1 then holderT + toPascalCase name
            else sprintf "%s%i" (toPascalCase name) i
        let t =
            //Exceptions...
            match t with
            | "Dir" -> "DirType"
            | "Form" -> "FormType"
            | "Object" -> "ObjectType"
            | "Style" -> "StyleType"
            | _ -> t
        match !generatedTypes |> Map.tryFind t with
        | Some vs when vs = values -> t, false
        | Some _ -> getGeneratedName (i+1) holderT name values
        | None ->
            generatedTypes := !generatedTypes |> Map.add t values
            generatedTypesByValues := !generatedTypesByValues |> Map.add values t
            t, true

let generateValueType holderT (name, values, desc) =
    let hasCase = values |> Array.exists (function Case _ -> true | _ -> false)
    if hasCase then
        let t, shouldEmitCode = getGeneratedName 0 holderT name (set values)
        (name, Some t), [
            if shouldEmitCode then
                let used = ref Map.empty
                let rec convert i x =
                    let name = 
                        if i = 0 then toPascalCase x
                        else sprintf "%s%i" (toPascalCase x) i
                    let name =
                        // Exceptions
                        match name with
                        | "Some" -> "Some_"
                        | "None" -> "None_"
                        | "Text" -> "Text_"
                        | "Tag" -> "Tag_"
                        | _ -> name
                    match !used |> Map.tryFind name with
                    | Some y when x = y -> name
                    | Some _ -> convert (i+1) x
                    | None ->
                        used := !used |> Map.add name x
                        name
                let toPascalCase = convert 0
                yield ""
                yield sprintf "type %s =" t
                for v in values do
                    match v with
                    | Empty -> failwith "Unexpected values combination."
                    | StringVariable name -> 
                        yield sprintf "    | %s of string" (toPascalCase name)
                    | NumberVariable name -> 
                        yield sprintf "    | %s of float" (toPascalCase name)
                    | PixelsVariable name -> 
                        yield sprintf "    | %s of int" (toPascalCase name)
                    | Case name ->
                        yield sprintf "    | %s" (toPascalCase name)
                yield sprintf "    member x.Value ="
                yield sprintf "        match x with"
                for v in values do
                    match v with
                    | Empty -> failwith "Unexpected values combination."
                    | StringVariable name ->
                        yield sprintf "        | %s %s -> %s" (toPascalCase name) (toCamelCase name) (toCamelCase name)
                    | NumberVariable name ->
                        yield sprintf "        | %s %s -> %s.ToString()" (toPascalCase name) (toCamelCase name) (toCamelCase name)
                    | PixelsVariable name ->
                        yield sprintf "        | %s %s -> %s.ToString()" (toPascalCase name) (toCamelCase name) (toCamelCase name)
                    | Case name ->
                        yield sprintf "        | %s -> \"%s\"" (toPascalCase name) (name.Replace("&quot;", ""))
        ]
    else
        match values with
        | [|StringVariable _|] -> (name, Some "string"), []
        | [|NumberVariable _|] -> (name, Some "float"), []
        | [|PixelsVariable _|] -> (name, Some "int"), []
        | [|Empty|] -> (name, None), []
        | _ -> failwith "Unexpected values combination."

let htmlAssembly =
    typeof<HTMLDivElement>.Assembly

let getTypeScriptTagClass tag typeScriptType =
    let tryFindType name =
        let t = htmlAssembly.GetType("FunScript.TypeScript." + name, false, true)
        if t = null then None
        else Some t.FullName
    let format tag = sprintf "HTML%sElement" tag
    defaultArg (
        [typeScriptType; format tag; format (toPascalCase tag); format(tag.ToUpperInvariant())]
        |> List.tryPick tryFindType)
        typeof<HTMLElement>.FullName

let generateTagCode (tag, isVoid, attributes, typeScriptType) =
    [
        yield ""
        yield ""
        let meth = if isVoid then "voidTag" else "nonVoidTag"
        let baseT = if isVoid then "IVoidElement" else "INonVoidElement"
        let t = "I" + toPascalCase tag + "Element"
        let valueTypes = attributes |> Array.map (generateValueType t)
        let typeLookup = valueTypes |> Array.map fst |> Map.ofArray
        for _, code in valueTypes do
            yield! code
        yield ""
        yield sprintf "type %s = inherit %s" t baseT
        yield ""
        yield sprintf "type %s() =" (toPascalCase tag)
        let tsT = getTypeScriptTagClass tag typeScriptType
        yield sprintf "    static member empty = %s \"%s\" : HtmlTag<%s, %s>" meth tag t tsT
        for name, values, desc in attributes do
            
            yield ""
            yield sprintf "    /// %s" desc
            let funName = toCamelCase name
            match typeLookup.[name] with
            | Some "string" ->
                yield sprintf "    static member %s = set<%s, %s> \"%s\"" funName t tsT name
            | Some "float" ->
                yield sprintf "    static member %s (x : float) = set<%s, %s> \"%s\" (x.ToString())" funName t tsT name
            | Some "int" ->
                yield sprintf "    static member %s (x : int) = set<%s, %s> \"%s\" (x.ToString())" funName t tsT name
            | Some vt ->
                yield sprintf "    static member %s (x : %s) = set<%s, %s> \"%s\" x.Value" funName vt t tsT name
            | None ->
                yield sprintf "    static member %s = setEmpty<%s, %s> \"%s\"" funName t tsT name
        yield ""
        let safeTag = if fsharpKeywords.Contains tag then tag + "Tag" else tag
        if isVoid then
            yield sprintf "let %s classes = %s.empty |> Element.classes classes" safeTag (toPascalCase tag)
        else
            yield sprintf "let %s classes children = %s.empty |> Element.classes classes |> Element.appendTags children" safeTag (toPascalCase tag)
            
    ]

let generateFile() =
    async {
        let! tags = findTags()
        let precursor = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "Header.fsx"))
        let code = tags |> Seq.collect generateTagCode |> String.concat Environment.NewLine
        let fullCode = precursor + code
        File.WriteAllText(Path.Combine(__SOURCE_DIRECTORY__, "Tags.fs"), fullCode)
    } |> Async.RunSynchronously