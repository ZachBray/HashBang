namespace Owin

open System.Runtime.CompilerServices
open Microsoft.Owin
open TypeInferred.HashBang.Owin

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
        appBuilder.Use<HashBangMiddleware>(hashBangOptions) |> ignore
