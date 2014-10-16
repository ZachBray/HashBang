namespace TypeInferred.HashBang.SignalR

open System
open System.Reflection
open System.Runtime.CompilerServices
open Microsoft.Owin
open Microsoft.AspNet.SignalR
open TypeInferred.HashBang.SignalR

type AppBuilderEx() =
    static member UseHashBangSignalR(appBuilder, serviceInstances) =
        GlobalHost.DependencyResolver.Register(typeof<ServiceProtocolConnection>, Func<obj>(fun () -> 
            upcast ServiceProtocolConnection(ServiceCollection(serviceInstances))))
        let route = typeof<ServiceProtocolConnection>.GetCustomAttribute<RouteAttribute>()
        Owin.OwinExtensions.MapSignalR(appBuilder, route.Path, typeof<ServiceProtocolConnection>, ConnectionConfiguration()) |> ignore

    