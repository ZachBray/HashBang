/// This module contains the scripts embedded in the client web/mobile application
module Chat.Client.Scripts

open TypeInferred.HashBang

type JQuery = ResourceProvider< "http://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.1/jquery.min.js" >
type SignalRJS = ResourceProvider< "http://ajax.aspnetcdn.com/ajax/signalr/jquery.signalr-2.0.3.min.js" >
type RxJs = ResourceProvider< "http://cdnjs.cloudflare.com/ajax/libs/rxjs/2.3.14/rx.all.min.js" >

let all = [
    JQuery.RawTextContents
    Bootstrap.Scripts.Bootstrap.RawTextContents
    Bootstrap.Scripts.Typeahead.RawTextContents
    SignalRJS.RawTextContents
    RxJs.RawTextContents
]