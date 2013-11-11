namespace System

module Disposable =
    
    let by f = { new IDisposable with member __.Dispose() = f() }

module Helpers =
    let inline tryConvert s =
        let mutable x = Unchecked.defaultof< ^a>
        if (^a: (static member TryParse: string * ^a byref -> bool) (s, &x))
            then Nullable x else Nullable()

open Helpers

module Int32 =
    let TryConvert str = tryConvert str : int Nullable

module UInt32 =
    let TryConvert str = tryConvert str : uint32 Nullable

module Double =
    let TryConvert str = tryConvert str : float Nullable

module Boolean =
    let TryConvert str = tryConvert str : bool Nullable

module Char =
    let TryConvert str = tryConvert str : char Nullable

module Decimal =
    let TryConvert str = tryConvert str : decimal Nullable

module Option =
    let bindNullable f x =
        match x with
        | Some y -> f y
        | None -> Nullable() 