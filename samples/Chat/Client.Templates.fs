[<FunScript.JS>]
module Chat.Client.Templates

open TypeInferred.HashBang.Html
open TypeInferred.HashBang.Html.Extensions
open Chat.Client
open Chat.Client.ViewModels
open Chat.Client.Stylesheets

module Shared =

    let ie8MediaFix = """<!-- HTML5 shim and Respond.js IE8 support of HTML5 elements and media queries -->
        <!--[if lt IE 9]>
          <script src="http://cdnjs.cloudflare.com/ajax/libs/html5shiv/3.7/html5shiv.js"></script>
          <script src="http://cdnjs.cloudflare.com/ajax/libs/respond.js/1.3.0/respond.js"></script>
        <![endif]-->""" 

    let head =
        head [] [
            yield upcast (title [] [] |> Element.appendText ["Chat Sample - HashBang"])

            yield upcast (meta [] |> Meta.name (Name.Generator "viewport")
                          |> Meta.content "width=device-width, initial-scale=1.0")

            for sheet in Stylesheets.all do
                yield style [] [] |> Style.``type`` IStyleElementType.Text_css
                      |> Element.appendText [ sheet ] :> _
        ]
        |> Element.appendText [ie8MediaFix]

    let headedPage(body:HtmlTag<IBodyElement, _>, funscript:HtmlTag<IScriptElement, _>) =
        html [] [
            yield upcast head
            yield upcast body
            for s in Scripts.all do
                yield upcast (script [] [] |> Element.appendText ["// <![CDATA[\n"; s; "\n// ]]>"] )
            yield upcast funscript
        ]

    let errorTemplate(error:string) =
        body [] [
            h1 [] [] |> Element.appendText ["Error"]
            p [] [] |> Element.appendText [error]
        ]

type NavBarPageTemplate(alerts : AlertsViewModel, auth : AuthenticationViewModel) =
    
    let (-->) element text = element |> Element.appendText [text]

    let navLink description uri currentUri =
        li [if uri = currentUri then yield Bootstrap.active] [
            a [] [] --> description |> A.href uri
        ]

    let navCommand description action =
        li [] [
            a [] [] --> description |> Element.onclick(fun _ -> action())
        ]

    let alertsSection() =
        div [] [] |> Element.appendSetUpByJQuery(fun q ->
            alerts.NewAlert |> Observable.subscribe (fun (alertType, title, error) ->
                let alertClass =
                    match alertType with
                    | Success -> Bootstrap.alert_success
                    | Info -> Bootstrap.alert_info
                    | Warning -> Bootstrap.alert_warning
                    | Danger -> Bootstrap.alert_danger
                div [Bootstrap.alert; alertClass; Bootstrap.alert_dismissible] [
                    button [] [
                        span [] [] |> Element.appendText ["&times;"]
                        |> Unchecked.set "aria-hidden" "true"

                        span [Bootstrap.sr_only] [] |> Element.appendText ["Close"]
                    ] 
                    |> Button.``type`` Type.Button
                    |> Element.``class`` Bootstrap.close
                    |> Unchecked.set "data-dismiss" "alert"

                    strong [] [] |> Element.appendText [title]
                ] 
                |> Element.appendText [error]
                |> q.append
                |> fun r -> r.Dispose() // Alerts should not set up any resources
                                        // so we get rid of them straight away.
            )
        )

    member __.Apply currentBaseUri title blurb children =
        body [] [
            div [Application.site_wrapper] [
                div [Application.site_wrapper_inner] [
                    div [Application.cover_container] [

                        div [Application.masthead; Bootstrap.clearfix] [
                            div [Application.inner] [
                                h3 [Application.masthead_brand] [] --> "Chat"
                                div [Bootstrap.nav; Application.masthead_nav] [
                                    if auth.IsLoggedIn then
                                        yield upcast navCommand "Log Out" auth.LogOut
                                    else
                                        // Here we use a type-safe uri builder to that ensures our link is correct
                                        // wherever we reference it.
                                        yield upcast navLink "Log In" (Routes.Session.LogIn.CreateUri()) currentBaseUri
                                        yield upcast navLink "Sign Up" (Routes.Session.SignUp.CreateUri()) currentBaseUri
                                ]
                            ]
                        ]

                        // Note: we took this template from: http://getbootstrap.com/examples/cover/#
                        //       and interestingly we found a "mistake" here using the type provider.
                        //       They reference a class cover-heading that isn't used.
                        div [Application.inner; Application.cover] [
                            h1 [] [] --> title

                            p [Bootstrap.lead] [] --> blurb

                            alertsSection()

                            div [] children
                        ]
                    
                        div [Application.mastfoot] [
                            div [Application.inner] [
                                p [] [] --> "Copyright 2014 Type Inferred Ltd."
                            ]
                        ]
                    ]
                ]
            ]
        ]