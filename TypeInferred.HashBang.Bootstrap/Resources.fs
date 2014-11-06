namespace TypeInferred.HashBang.Bootstrap

open FunScript
open TypeInferred.HashBang

type Extension = string
type Replacement = string

[<JS>]
type FontDefinition = 
    | FontDefinition of Map<Extension, Replacement>
    
    member font.ReplaceUri(uriWithoutExtension : string, css : string) =
        let (FontDefinition formats) = font
        formats |> Map.fold (fun (acc : string) ext replacement ->
            acc.Replace(uriWithoutExtension + ext, replacement)) css
[<JS>]
module Fonts =
    type Glyphicons = ResourceProvider< "http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/fonts/glyphicons-halflings-regular.woff" >
    let glyphicons = 
        FontDefinition(
            Map [
                ".eot", "http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/fonts/glyphicons-halflings-regular.eot"
                ".ttf", "http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/fonts/glyphicons-halflings-regular.ttf"
                ".svg", "http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/fonts/glyphicons-halflings-regular.svg"
                ".woff", Glyphicons.RawDataUri
            ])

    type FontAwesome =  ResourceProvider< "http://maxcdn.bootstrapcdn.com/font-awesome/4.2.0/fonts/fontawesome-webfont.woff" >

    let fontAwesome = 
        FontDefinition(
            Map [
                ".eot", "http://maxcdn.bootstrapcdn.com/font-awesome/4.2.0/fonts/fontawesome-webfont.eot"
                ".ttf", "http://maxcdn.bootstrapcdn.com/font-awesome/4.2.0/fonts/fontawesome-webfont.ttf"
                ".svg", "http://maxcdn.bootstrapcdn.com/font-awesome/4.2.0/fonts/fontawesome-webfont.svg"
                ".woff", FontAwesome.RawDataUri
            ])

[<JS>]
module Stylesheets =
    /// Static stylesheet from CDN (unmonitored)
    type Bootstrap = CssClassesProvider< "http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css", ShouldMonitorChanges=false >
    
    let rawBootstrapWithEmeddedFont = Fonts.glyphicons.ReplaceUri("../fonts/glyphicons-halflings-regular", Bootstrap.RawStyleSheet)

    type FontAwesome = CssClassesProvider< "http://maxcdn.bootstrapcdn.com/font-awesome/4.2.0/css/font-awesome.min.css", ShouldMonitorChanges=false >

    let rawFontAwesomeWithEmbeddedFont = Fonts.fontAwesome.ReplaceUri("../fonts/fontawesome-webfont", FontAwesome.RawStyleSheet)

    
[<JS>]
module Scripts = 
    type Bootstrap = ResourceProvider< "http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js" >

    type Typeahead = ResourceProvider< "http://cdnjs.cloudflare.com/ajax/libs/typeahead.js/0.10.4/typeahead.bundle.min.js" >
    