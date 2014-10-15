namespace Owin

open System
open System.Reflection
open System.Runtime.CompilerServices
open Microsoft.Owin
open Microsoft.AspNet.SignalR
open TypeInferred.HashBang.SignalR

[<Extension>]
type AppBuilderExtensions() =
    [<Extension>]
    static member UseHashBangSignalR(appBuilder:IAppBuilder, [<ParamArray>]serviceInstances) =
        GlobalHost.DependencyResolver.Register(typeof<ServiceProtocolConnection>, Func<obj>(fun () -> 
            upcast ServiceProtocolConnection(ServiceCollection(serviceInstances))))
        let route = typeof<ServiceProtocolConnection>.GetCustomAttribute<RouteAttribute>()
        appBuilder.MapSignalR(route.Path, typeof<ServiceProtocolConnection>, ConnectionConfiguration()) |> ignore

    