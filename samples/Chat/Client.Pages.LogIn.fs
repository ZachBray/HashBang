namespace Chat.Client.Pages

open TypeInferred.HashBang
open TypeInferred.HashBang.Html
open Chat.Client
open Chat.Client.Stylesheets

[<FunScript.JS>]
type LogInPage() =
    
    let currentUri = Routes.Session.LogIn.CreateUri() 

    let textInput placeholder inputType addons = 
        !![Bootstrap.input_group; Bootstrap.col_lg_6; Bootstrap.col_lg_offset_3] ==> [
            Span.empty +. [Bootstrap.input_group_addon] 
            |> Element.style "width: 20%;" |> Element.append addons
            Input.empty +. [Bootstrap.form_control] |> Input.``type`` inputType
            |> Input.placeholder placeholder
        ]

    let createPage() =
        Templates.insideNavBar
            currentUri
            "Log In" 
            "Please fill in your email address and password or sign-up for a new account." [
            P.empty ==> [textInput "joebloggs@gmail.com" IInputElementType.TextCase [Text "Email"]]
            P.empty ==> [textInput "P4ssW0rD" IInputElementType.Password [Text "Pass"]]
        ]

    interface IPage with
        member __.TryHandle request = 
            Routes.Session.LogIn.CreateHandler (fun ps -> 
                async { return Some(createPage()) }
            ) request.Path request.QueryParams