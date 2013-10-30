namespace System

module Disposable =
    
    let by f = { new IDisposable with member __.Dispose() = f() }