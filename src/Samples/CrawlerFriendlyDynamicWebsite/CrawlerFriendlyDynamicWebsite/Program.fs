[<ReflectedDefinition>]
module CrawlerFriendlyDynamicWebsite

// Here we show a very simple example of how we can define handlers for routes that
// can be executed on the client or the server. This allows the website to be
// highly dynamic but also allows crawlers to index the page.
// See: https://support.google.com/webmasters/answer/174992?hl=en

// Here we open up some namespaces.
open System
open TypeInferred.HashBang
open TypeInferred.HashBang.Html

// Here we define the routes that may be used inside the application.
// The first column specifies the url format. This can include fixed segments,
// variable segments, query parameters and optional query parameters.
// The second column specifies the "resource" and "action". These are used
// to generate the type tree in a nice way. The third column supplies documentation.
type Routes = TypeInferred.HashBang.ClientRoutesProvider<"
/               # Home.Open     # The home page
/page?id=int    # Page.Nth      # The nth page
">

// Here we define what the head of the HTML document will look like using the
// HashBang Html DSL.
let headTemplate =
    Head.empty
    |> Element.appendTags [
        Title.empty |> Element.appendText ["CrawlerFriendlyDynamicWebsite"]

        Meta.empty |> Meta.name (Name.Generator "viewport")
        |> Meta.content "width=device-width, initial-scale=1.0"

        //TODO: CSS
    ]
    |> Element.appendText [
        """
    <!-- HTML5 shim and Respond.js IE8 support of HTML5 elements and media queries -->
    <!--[if lt IE 9]>
      <script src="js/html5shiv.js"></script>
      <script src="js/respond.min.js"></script>
    <![endif]-->
        """
    ]

// Here we define the sections of our template that are changed when
// the url hash changes.
module TemplateSections =
    let navbar = "navbar_section"
    let content = "content_section"

// Here we define a template for the body of our application.
let bodyTemplate =
    Body.empty
    |> Element.appendTags [
        
        // We include a placeholder for each template section.
        Div.empty |> Element.id TemplateSections.navbar
        |> Element.appendText ["Navigation will go here!"]

        Div.empty |> Element.id TemplateSections.content
        |> Element.appendText ["Content will go here!"]

        Script.empty |> Script.src "http://code.jquery.com/jquery.min.js"
    ]

// Here we define handlers to update the content section. Each handler is 
// defined for a particular url hash format. E.g., "#!/page?id=int".
// These formats are defined above via a type provider that emits the Routes type.
// Each handler returns HTML + JS (via an IHtmlTag) that will replace the content of the section.
// This HTML can be rendered on the client or on the server for crawlers.
// See: https://support.google.com/webmasters/answer/174992?hl=en
let contentHandlers =
    [
        Routes.Home.Open.CreateHandler(fun ps ->
            async {
                return
                    Div.empty
                    |> Element.appendText ["Home Page!"]
                    |> Element.appendTags [
                        // We can use the Routes type for type-safe url hash creation.
                        A.empty |> A.href (Routes.Page.Nth.CreateUri 1)
                        |> Element.appendText ["Page 1"]
                    ] :> IHtmlTag
                    |> Some
            })

        Routes.Page.Nth.CreateHandler(fun ps ->
            async {
                return Div.empty
                    |> Element.appendText ["Page #" + ps.id.ToString()]
                    |> Element.appendTags [
                        // We can use the Routes type for type-safe url hash creation.
                        A.empty |> A.href (Routes.Page.Nth.CreateUri (ps.id + 1))
                        |> Element.appendText ["Page " + (ps.id + 1).ToString()]
                    ] :> IHtmlTag
                    |> Some
            })
    ]

// Here we define a mapping from content section to the sections handlers.
let sections =
    Map [
        TemplateSections.content, contentHandlers
    ]

// Here we open up the port for non-admin VS instances.
System.Net.Netsh.addUrlAcl "http://*:8081/"

// Here we define the address of the website and its
// parts that we have defined above.
DynamicWebsite.At "http://*:8081/"
|> DynamicWebsite.WithHeadTemplate headTemplate
|> DynamicWebsite.WithBodyTemplate bodyTemplate
// For the sections map we need to pass in a Quotation Expression.
// This is so that it can be compiled down to JavaScript for client-side
// rendering.
|> DynamicWebsite.WithSections <@ sections @>
|> DynamicWebsite.Start
|> ignore

/// Typical user (client-side) rendering...
System.Diagnostics.Process.Start("http://localhost:8081/") |> ignore

/// Crawler (server-side) rendering...
System.Diagnostics.Process.Start("http://localhost:8081/?_escaped_fragment_=%2Fpage%3Fid%3D2") |> ignore

printfn "Press return to kill server!"
Console.ReadLine() |> ignore