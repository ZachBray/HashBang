[<FunScript.JS>]
module Chat.Domain.Validation

let isEmail (text : string) =
    let atIndex = text.IndexOf "@"
    let lastAtIndex = text.LastIndexOf "@"
    let dotIndex = text.LastIndexOf "."
    atIndex > 0 
    && atIndex = lastAtIndex 
    && dotIndex > atIndex 
    && dotIndex < text.Length - 1
    