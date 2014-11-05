module Chat.Container

open SimpleInjector
open TypeInferred.HashBang
open Chat.Server
open Chat.Client.ViewModels
open Chat.Client.Templates
open Chat.Client.Pages

let private container =
    lazy 
        let container = Container()

        // Services
        let authentication = Lifestyle.Singleton.CreateRegistration<AuthenticationService>(container)
        container.AddRegistration(typeof<IAuthenticationService>, authentication)
        container.RegisterAll(typeof<IService>, authentication)

        // View Models
        container.Register<AlertsViewModel>(Lifestyle.Singleton)
        container.Register<AuthenticationViewModel>(Lifestyle.Singleton)

        // Pages + Templates
        container.Register<NavBarPageTemplate>(Lifestyle.Singleton)
        container.RegisterAll(typeof<IPage>, 
            [|
                Lifestyle.Singleton.CreateRegistration<LogInPage>(container)
                Lifestyle.Singleton.CreateRegistration<SignUpPage>(container)
                Lifestyle.Singleton.CreateRegistration<ConversationPage>(container)
            |])

        // Verification
        container.Verify()
        container

let createPages() = container.Value.GetAllInstances<IPage>() |> Seq.toList
let createServices() = container.Value.GetAllInstances<IService>() |> Seq.toList

let isRegisteredAsSingleton t = container.Value.GetRegistration(t).Lifestyle = Lifestyle.Singleton


