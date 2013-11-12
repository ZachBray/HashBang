#r @"..\..\lib\ExCss.dll"

open ExCSS
open System
open System.IO


let parseClasses(filePath) =
    let parser = Parser()
    let content = File.ReadAllText filePath
    let sheet = parser.Parse content
    //sheet.Errors |> Seq.toArray |> printfn "Errors: %A"
    sheet.Rulesets |> Seq.collect (fun rule ->
        rule.Value.Split(',') |> Seq.map (fun section ->
            section.Split(':','>','<','+','[',']',';',' ') 
            |> Seq.map (fun term -> term.Trim()) 
            |> Seq.head))
    |> Seq.filter ((<>) "")
    |> Seq.filter (fun term -> term.StartsWith ".")
    |> Seq.map (fun x -> x.Substring 1)
    |> set

let parseClassesLighweight(filePath) =
    let content = File.ReadAllText filePath
    content.Split(
        [|' ';'\r';'\n';'\t';',';'{';'}';':';'>';'<';'+';'[';']';';'|], 
        StringSplitOptions.RemoveEmptyEntries)
    |> Seq.filter (fun term -> term.StartsWith ".")
    |> Seq.collect (fun classes -> classes.Split([|'.'|], StringSplitOptions.RemoveEmptyEntries))
    |> set

let test() =
    let relativePath = @"..\Samples\Chat\ChatExample.Client\www\css\bootstrap.min.css"
    let absolutePath = Path.Combine(__SOURCE_DIRECTORY__, relativePath)
    let fromLibrary = parseClasses absolutePath
    let fromLightweight = parseClassesLighweight absolutePath
    printfn "Lighweight baby? %b" (fromLibrary = fromLightweight)
    if fromLibrary <> fromLightweight then
        let missing = Set.toArray (fromLibrary - fromLightweight)
        printfn "Missing: %A" missing
        let extra = Set.toArray (fromLightweight - fromLibrary)
        printfn "Extra: %A" extra