#r "System.Xml"
#r "System.Xml.Linq"
#r @"..\..\lib\HtmlAgilityPack.dll"

open System
open System.IO
open System.Net
open HtmlAgilityPack

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

//TODO: Factor this out!

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
            "use"; "let"; "do"; "yield"; "lazy"; "constructor"; "base" |]

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
       .Replace("+", "_")
       .Replace(",", "_")

let numerals = Set [|'0';'1';'2';'3';'4';'5';'6';'7';'8';'9'|]

let postClean(str : string) =
    match str with
    | "" -> "Empty"
    | x when fsharpKeywords.Contains x -> sprintf "``%s``" x
    | _ when numerals.Contains(str.[0]) -> "_" + str
    | _ -> str

let toPascalCase name =
    name |> clean |> String.mapi (fun i c ->
        match i, Char.IsUpper c with
        | 0, false -> Char.ToUpper c
        | _ -> c)
    |> postClean

let toCamelCase name =
    name |> clean |> String.mapi (fun i c ->
        match i, Char.IsUpper c with
        | 0, true -> Char.ToLower c
        | _ -> c)
    |> postClean 

let extractTuples node =
    node |> choose (fun n ->
        if n.Name = "tr" then
            let cells =
                [| for x in n.ChildNodes -> x |]
                |> Array.filter (fun n -> n.Name = "td")
                |> Array.map (fun n -> n.InnerText.Trim())
            if cells.Length <> 4 || 
               cells.[1].Contains "mac-" || 
               cells.[1].Contains "macbinary" ||
               cells.[1].Contains "\n" 
            then None
            else Some(cells.[0], cells.[1], cells.[2])
        else None)

let generateCode() =
    async {
        let! text = downloadAsync "http://www.freeformatter.com/mime-types-list.html"
        let html = HtmlDocument()
        html.LoadHtml text
        let pairs = 
            extractTuples html.DocumentNode
            |> Seq.collect (fun (x, y, zs) ->
                zs.Split([|' '; ','|], StringSplitOptions.RemoveEmptyEntries)
                |> Seq.map (fun z -> x, y, z))
        return 
            seq {
                yield ""
                yield "module ContentTypes ="
                yield "    let lookupByExtension ="
                yield "        Map [|"
                for doc, t, ext in pairs do
                    yield sprintf "            \"%s\", ContentType \"%s\"" ext t
                yield "        |]"
                yield ""
                yield "    let fromExtension ext = lookupByExtension.TryFind ext"
                yield ""
                let modules =
                    pairs |> Seq.distinctBy (fun (_, t, _) -> t)
                    |> Seq.groupBy (fun (_, t, _) -> toPascalCase(t.Split([|'/'|]).[0]))
                for m, xs in modules do
                    yield sprintf "    module %s =" m
                    for doc, t, _ in xs do
                        let n = toCamelCase(t.Split([|'/'|]).[1])
                        yield sprintf "        /// \"%s\": %s" t doc
                        yield sprintf "        let %s = ContentType \"%s\"" n t
                        yield ""
            } |> String.concat "\r\n"
    }

let generateFile() =
    async {
        let! code = generateCode()
        let precursor = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "ContentTypesHeader.fsx"))
        let fullCode = precursor + code
        File.WriteAllText(Path.Combine(__SOURCE_DIRECTORY__, "Runtime.fs"), fullCode)
    } |> Async.RunSynchronously