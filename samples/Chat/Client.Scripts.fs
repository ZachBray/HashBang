/// This module contains the scripts embedded in the client web/mobile application
module Chat.Client.Scripts

open TypeInferred.HashBang


type JQuery = ResourceProvider< "../../lib/JQuery/jquery-2.1.1.min.js" >
type SignalRJS = ResourceProvider< "../../lib/JQuery/jquery.signalr-2.0.3.min.js" >
type RxJs = ResourceProvider< "../../lib/RxJS/rx.all.min.js" >

let all = [
    JQuery.RawTextContents
    Bootstrap.Scripts.Bootstrap.RawTextContents
    Bootstrap.Scripts.Typeahead.RawTextContents
    SignalRJS.RawTextContents
    RxJs.RawTextContents
]