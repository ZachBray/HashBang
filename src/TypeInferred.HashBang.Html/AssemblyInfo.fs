namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("TypeInferred.HashBang.Html")>]
[<assembly: AssemblyProductAttribute("HashBang")>]
[<assembly: AssemblyDescriptionAttribute("A collection of libraries for building web and mobile applications. Leverages FunScript for seamless full-stack development in F#. Adheres to Google's standard for ajax=crawling.")>]
[<assembly: AssemblyVersionAttribute("0.2.0")>]
[<assembly: AssemblyFileVersionAttribute("0.2.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.2.0"
