namespace Chat.Client.Pages

open FunScript.TypeScript
open TypeInferred.HashBang
open TypeInferred.HashBang.Html
open Chat.Client
open Chat.Client.Stylesheets

[<FunScript.JS>]
type LogInPage() =
    
    let currentUri = Routes.Session.LogIn.CreateUri() 

    let textInput placeholder inputType addons = 
        div [Bootstrap.input_group; Bootstrap.col_lg_6; Bootstrap.col_lg_offset_3] [
            span [Bootstrap.input_group_addon] []
            |> Element.style "width: 20%;" |> Element.append addons
            input [Bootstrap.form_control] |> Input.``type`` inputType
            |> Input.placeholder placeholder
            |> Element.appendSetUp(fun input -> 
                input.value <- "typed set-up!"
                Disposable.empty)
        ]

    let createPage() =
        Templates.insideNavBar
            currentUri
            "Log In" 
            "Please fill in your email address and password or sign-up for a new account." [
            p [] [textInput "joebloggs@gmail.com" IInputElementType.Text_ [Text "Email"]]
            p [] [textInput "P4ssW0rD" IInputElementType.Password [Text "Pass"]]
        ]

    interface IPage with
        member __.TryHandle request = 
            Routes.Session.LogIn.CreateHandler (fun ps -> 
                async { return Some(createPage()) }
            ) request.Path request.QueryParams