namespace TypeInferred.HashBang.Bootstrap

open FunScript
open TypeInferred.HashBang

module Literals =
    [<Literal>]
    let directory = __SOURCE_DIRECTORY__

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
    type Glyphicons = ResourceProvider< "../../../lib/Bootstrap/glyphicons-halflings-regular.woff", Literals.directory >
    let glyphicons = 
        FontDefinition(
            Map [
                ".eot", "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.0/fonts/glyphicons-halflings-regular.eot"
                ".ttf", "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.0/fonts/glyphicons-halflings-regular.ttf"
                ".svg", "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.0/fonts/glyphicons-halflings-regular.svg"
                ".woff", Glyphicons.RawDataUri
            ])

    type FontAwesome =  ResourceProvider< "../../../lib/FontAwesome/fontawesome-webfont.woff", Literals.directory >

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
    type Bootstrap = CssClassesProvider< "../../../lib/Bootstrap/bootstrap.min.css", ShouldMonitorChanges=false, Directory=Literals.directory >
    
    let rawBootstrapWithEmeddedFont = Fonts.glyphicons.ReplaceUri("../fonts/glyphicons-halflings-regular", Bootstrap.RawStyleSheet)

    type FontAwesome = CssClassesProvider< "../../../lib/FontAwesome/font-awesome.min.css", ShouldMonitorChanges=false, Directory=Literals.directory >

    let rawFontAwesomeWithEmbeddedFont = Fonts.fontAwesome.ReplaceUri("../fonts/fontawesome-webfont", FontAwesome.RawStyleSheet)

    
[<JS>]
module Scripts = 
    type Bootstrap = ResourceProvider< "../../../lib/Bootstrap/bootstrap.min.js", Literals.directory >

    type Typeahead = ResourceProvider< "../../../lib/Typeahead/typeahead.bundle.js", Literals.directory > //TODO: get min version
    