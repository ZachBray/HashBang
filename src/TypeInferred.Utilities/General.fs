[<AutoOpen; ReflectedDefinition>]
module Utilities

open System
open System.IO
open System.Collections.Concurrent

let memoize f  =
    let cache = ConcurrentDictionary()
    let factory = Func<_,_>(fun x -> f x)
    fun x -> cache.GetOrAdd(key = x, valueFactory = factory)

let memoizeRec f  =
    let cache = ConcurrentDictionary()
    let rec getCached x =
        cache.GetOrAdd(key = x, valueFactory = Func<_,_>(fun x -> f getCached x))
    fun x -> getCached x

let memoizeRecLazy f  =
    let getCached = memoizeRec (fun g x -> lazy f g x)
    fun x -> (getCached x).Value

let defaultArgAsync optionalValue defaultValue =
    async {
        match optionalValue with
        | None -> return defaultValue
        | Some x -> return! x
    }

//let currentExePath =
//    lazy System.Reflection.Assembly.GetEntryAssembly().Location
//
//let currentExeDirectory =
//    lazy Path.GetDirectoryName currentExePath.Value