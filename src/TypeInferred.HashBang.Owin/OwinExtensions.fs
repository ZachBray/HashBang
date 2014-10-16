namespace Owin

open System.Runtime.CompilerServices
open Microsoft.Owin
open TypeInferred.HashBang
open TypeInferred.HashBang.Owin
open TypeInferred.HashBang.SignalR

[<Extension>]
type AppBuilderExtensions() =

    [<Extension>]
    static member UseCaching<'Html>(appBuilder:IAppBuilder) =
        appBuilder.Use<CachingMiddleware>() |> ignore

    [<Extension>]
    static member UseCompression<'Html>(appBuilder:IAppBuilder) =
        appBuilder.Use<CompressionMiddleware>() |> ignore

    [<Extension>]
    static member UseHashBang<'Html>(appBuilder:IAppBuilder, hashBangOptions:HashBangOptions) =
        let hashBangOptions = 
            { hashBangOptions with 
                FunScriptComponentInjector = fun cs -> 
                    TypeInferred.HashBang.SignalR.Interop.components 
                    @ hashBangOptions.FunScriptComponentInjector cs  }
        appBuilder.Use<HashBangMiddleware>(hashBangOptions) |> ignore
        AppBuilderEx.UseHashBangSignalR(appBuilder, hashBangOptions.Services)