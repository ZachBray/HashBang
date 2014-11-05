module Chat.Program

open System
open Owin
open Microsoft.Owin
open Microsoft.Owin.Hosting
open TypeInferred.HashBang

let settings =
    {
        /// Note: this is only a template for the first rendering of the page.
        ServerServedPageTemplate = Client.Templates.headedPage
        ErrorPageTemplate = <@ Client.Templates.errorTemplate @>
        Pages = Container.createPages()
        Services = Container.createServices()
        Advanced = 
        { 
            AdvancedHashbangOptions.Default with 
#if DEBUG
                IsJavaScriptCompressionEnabled = false
#else
                IsJavaScriptCompressionEnabled = true
#endif
        }
    }

[<EntryPoint>]
let main args =
    try
        let url = "http://localhost:8081/"
        use app = WebApp.Start(url, fun appBuilder ->

            
#if DEBUG   // Error page for debug
            appBuilder.UseErrorPage(Diagnostics.ErrorPageOptions(ShowExceptionDetails=true)) |> ignore
#endif
            appBuilder.UseCaching()
            appBuilder.UseCompression()
            appBuilder.UseHashBang(settings)
        )
        System.Diagnostics.Process.Start("http://localhost:8081/") |> ignore
        Console.ReadLine() |> ignore
        0
    with ex -> 
        Console.WriteLine(ex)
        1