module Chat.Container

open SimpleInjector
open TypeInferred.HashBang
open Chat.Server
open Chat.Client.Pages

let private container =
    lazy 
        let container = Container()
        let authentication = Lifestyle.Singleton.CreateRegistration<AuthenticationService>(container)
        container.AddRegistration(typeof<IAuthenticationService>, authentication)
        container.RegisterAll(typeof<IService>, authentication)
        let logInPage = Lifestyle.Singleton.CreateRegistration<LogInPage>(container)
        container.RegisterAll(typeof<IPage>, logInPage)
        container.Verify()
        container

let createPages() = container.Value.GetAllInstances<IPage>() |> Seq.toList
let createServices() = container.Value.GetAllInstances<IService>() |> Seq.toList


