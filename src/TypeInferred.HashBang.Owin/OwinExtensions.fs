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
    static member UseHashBang<'Html>(appBuilder:IAppBuilder, options:HashBangOptions) =
        let hashBangOptions = 
            { options with
                Advanced =
                    { options.Advanced with
                        FunScriptComponentInjector = fun cs -> 
                            TypeInferred.HashBang.SignalR.Interop.components 
                            @ options.Advanced.FunScriptComponentInjector cs  } }
        appBuilder.Use<HashBangMiddleware>(hashBangOptions) |> ignore
        AppBuilderEx.UseHashBangSignalR(appBuilder, hashBangOptions.Services)