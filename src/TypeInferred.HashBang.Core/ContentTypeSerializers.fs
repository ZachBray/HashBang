[<AutoOpen>]
module TypeInferred.HashBang.ContentTypeSerializers

open System.Text

open TypeInferred.HashBang.Runtime

type ContentTypeSerializer<'ResponseType> =
    {
        ContentType : ContentType
        Serialize : 'ResponseType -> byte[]
        Deserialize : byte[] -> 'ResponseType
    }

let Json() =
    {
        ContentType = ContentTypes.Application.json
        Serialize = 
            let toJson = precomputeToJson() 
            toJson >> Encoding.UTF8.GetBytes
        Deserialize = 
            let fromJson = precomputeFromJson()
            Encoding.UTF8.GetString >> fromJson
    }

let Text contentType () =
    {
        ContentType = contentType
        Serialize = (Encoding.UTF8.GetBytes : string -> byte[])
        Deserialize = Encoding.UTF8.GetString
    }
    
let PlainText = Text ContentTypes.Text.plain
let Html = Text ContentTypes.Text.html
let Css = Text ContentTypes.Text.css
let JavaScript = Text ContentTypes.Application.javascript

let Binary contentType () =
    {
        ContentType = contentType
        Serialize = id
        Deserialize = id
    }

let OctetStream = Binary ContentTypes.Application.octet_stream