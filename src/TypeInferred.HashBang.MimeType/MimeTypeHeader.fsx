namespace TypeInferred.HashBang

[<ReflectedDefinition>]
type MimeType = 
    | MimeType of string
    member x.Mime = 
        let (MimeType mime) = x
        mime