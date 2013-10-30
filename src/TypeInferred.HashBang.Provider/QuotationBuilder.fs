// ----------------------------------------------------------------------------------------------
// Utilities for building F# quotations without quotation literals
// ----------------------------------------------------------------------------------------------

module ProviderImplementation.QuotationBuilder

open System
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Reflection
open Samples.FSharp.ProvidedTypes

/// Dynamic operator (?) that can be used for constructing quoted F# code without 
/// quotations (to simplify constructing F# quotations in portable libraries - where
/// we need to pass the System.Type of various types as arguments)
///
/// There are two possible uses:
///    typ?Name tyArgs args
///    typ?Name args
///
/// In the first case, tyArgs is a sequence of type arguments for method `Name`.
/// Actual arguments can be either expression (Expr<'T>) or primitive values, whic
/// are automatically wrapped using Expr.Value.
///
let (?) (typ:Type) (operation:string) (args1:'T) : 'R = 
  // Arguments are either Expr or other type - in the second case,
  // we treat them as Expr.Value (which will only work for primitives)
  let convertValue (arg:obj) = 
    match arg with
    | :? Expr as e -> e
    | value -> Expr.Value(value, value.GetType())

  let invokeOperation (tyargs:obj, tyargsT) (args:obj, argsT) =
    // To support (e1, e2, ..) syntax, we use tuples - extract tuple arguments
    // First, extract type arguments - a list of System.Type values
    let tyargs = 
      if tyargsT = typeof<unit> then []
      elif FSharpType.IsTuple(tyargsT) then
        [ for f in FSharpValue.GetTupleFields(tyargs) -> f :?> Type ]
      else [ tyargs :?> Type ]
    // Second, extract arguments (which are either Expr values or primitive constants)
    let args = 
      if argsT = typeof<unit> then []
      elif FSharpType.IsTuple(argsT) then
        [ for f in FSharpValue.GetTupleFields(args) -> convertValue f ]
      else [ convertValue args ]

    // Find a method that we want to call
    let flags = BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.Instance
    match typ.GetMember(operation, MemberTypes.All, flags) with 
    | [| :? MethodInfo as mi |] -> 
        let mi = 
          if tyargs = [] then mi
          else mi.MakeGenericMethod(tyargs |> Array.ofList)
        if mi.IsStatic then Expr.Call(mi, args)
        else Expr.Call(List.head args, mi, List.tail args)
    | [| :? ConstructorInfo as ci |] ->
        if tyargs <> [] then failwith "Constructor cannot be generic!"
        Expr.NewObject(ci, args)
    | [| :? PropertyInfo as pi |] ->
        let isStatic = 
          pi.CanRead && pi.GetGetMethod().IsStatic || 
          pi.CanWrite && pi.GetSetMethod().IsStatic
        if isStatic then Expr.PropertyGet(pi, args)
        else Expr.PropertyGet(List.head args, pi, List.tail args)
    | options -> failwithf "Constructing call of the '%s' operation failed. Got %A" operation options

  // If the result is a function, we are called with two tuples as arguments
  // and the first tuple represents type arguments for a generic function...
  if FSharpType.IsFunction(typeof<'R>) then
    let domTyp, res = FSharpType.GetFunctionElements(typeof<'R>)
    if res <> typeof<Expr> then failwith "QuotationBuilder: The resulting type must be Expr!"
    FSharpValue.MakeFunction(typeof<'R>, fun args2 ->
      invokeOperation (args1, typeof<'T>) (args2, domTyp) |> box) |> unbox<'R>
  else invokeOperation ((), typeof<unit>) (args1, typeof<'T>) |> unbox<'R>

[<ReflectedDefinition>]
type AsyncExtensions() =
    static member Map<'a, 'b>(xAsync : 'a Async, f : Func<'a, 'b>) =
        async {
            let! x = xAsync
            return f.Invoke x
        }

let makeConverter (f:Expr -> Expr) argType = 
    let var = Var("t", argType)
    let convBody = f (Expr.Var var)
    convBody.Type, Expr.NewDelegate(typedefof<Func<_,_>>.MakeGenericType [| argType; convBody.Type |], [var], convBody)

// Generating code for async is more tricky, because we need to perform
// the mapping not on the result (as above) but inside async block. So we
// generate function and apply 'ApiaryGenerationHelper.AsyncMap(work, f)'
let typeAsync (resultTy : Type) (asyncWork:Expr)  =
    let resultTy = 
        // If we just used 'resultTy' (as it is), then the method information
        // returned by MakeGenericMethod cannot be used in Expr.Call, because
        // it does not support GetParameters method (doh!)
        //
        // We emulate the F# type provider type erasure mechanism to get the 
        // actual (erased) type and use _that_ as our generic type (because
        // that is System.RuntimeType). We erase ProvidedTypes to their base
        // and we erase array of provided type to array of base type.
        match resultTy with 
        | :? ProvidedTypeDefinition -> resultTy.BaseType 
//        | :? ProvidedSymbolType as sym ->
//            match sym.Kind, sym.Args with
//            | SymbolKind.SDArray, [typ] ->
//                typ.BaseType.MakeArrayType()
//            | _ -> failwith "asyncMap: Unsupported ProvidedSymbolType" 
        | _ -> resultTy

    let _, convFunc = makeConverter (fun e -> Expr.Coerce(e, resultTy)) typeof<obj>
    typeof<AsyncExtensions>?Map (typeof<obj>, resultTy) (asyncWork, convFunc)