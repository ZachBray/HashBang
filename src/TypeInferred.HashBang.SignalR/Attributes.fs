namespace TypeInferred.HashBang.SignalR

open System

[<AttributeUsage(AttributeTargets.Class, AllowMultiple=false)>]
type internal RouteAttribute(path : string) =
    inherit Attribute()
    member val Path = path

[<AttributeUsage(AttributeTargets.Class, AllowMultiple=false)>]
type ServiceAttribute() =
    inherit Attribute()

[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple=false)>]
type ClientCannotAccessAttribute(path : string) =
    inherit Attribute()
    member val Path = path