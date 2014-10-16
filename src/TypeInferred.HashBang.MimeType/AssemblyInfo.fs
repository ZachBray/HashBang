namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("TypeInferred.HashBang.MimeType")>]
[<assembly: AssemblyProductAttribute("HashBang")>]
[<assembly: AssemblyDescriptionAttribute("A collection of libraries for building web and mobile applications. Leverages FunScript for seamless full-stack development in F#. Adheres to Google's standard for ajax=crawling.")>]
[<assembly: AssemblyVersionAttribute("0.3.1")>]
[<assembly: AssemblyFileVersionAttribute("0.3.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.3.1"
