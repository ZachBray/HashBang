namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("HashBang")>]
[<assembly: AssemblyProductAttribute("HashBang")>]
[<assembly: AssemblyDescriptionAttribute("A collection of libraries for building web and mobile applications. Leverages FunScript for seamless full-stack development in F#. Adheres to Google's standard for ajax=crawling.")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
