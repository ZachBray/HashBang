/// This module contains the stylesheets embedded in the client web/mobile application
module Chat.Client.Stylesheets

open TypeInferred.HashBang

/// Local stylesheet (monitored)
type Application = CssClassesProvider< "../Chat/App.css", ShouldMonitorChanges=true >
/// Static stylesheet from CDN (unmonitored)
type Bootstrap = Bootstrap.Stylesheets.Bootstrap
//CssClassesProvider< "http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css", ShouldMonitorChanges=false >

let all = [Bootstrap.Stylesheets.rawBootstrapWithEmeddedFont; Bootstrap.Stylesheets.rawFontAwesomeWithEmbeddedFont; Application.RawStyleSheet]