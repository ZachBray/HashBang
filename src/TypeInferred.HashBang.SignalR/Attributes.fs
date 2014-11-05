namespace TypeInferred.HashBang

open System

type IService = interface end

module ServiceEx =
    let private iserviceName = typeof<IService>.FullName

    let isAHashBangService(t : Type) =
        t.GetInterfaces() |> Seq.exists (fun t -> t.FullName = iserviceName)

namespace TypeInferred.HashBang.SignalR

open System

[<AttributeUsage(AttributeTargets.Class, AllowMultiple=false)>]
type internal RouteAttribute(path : string) =
    inherit Attribute()
    member val Path = path

[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple=false)>]
type ClientCannotAccessAttribute() =
    inherit Attribute()