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
                    | Good -> Bootstrap.alert_success
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

    let navbarToggleButton (navLinks : IHtmlTag) =
        button [Bootstrap.navbar_toggle] [
            span [Bootstrap.sr_only] [] |> Element.appendText ["Toggle navigation"]
            span [Bootstrap.icon_bar] []
            span [Bootstrap.icon_bar] []
            span [Bootstrap.icon_bar] []
        ] 
        |> Element.appendClass "collapsed" //?
        |> Unchecked.set "data-toggle" "collapse"
        |> Unchecked.set "data-target" ("#" + navLinks.Id)

    (*
        <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
      <ul class="nav navbar-nav">
        <li class="active"><a href="#">Link</a></li>
        <li><a href="#">Link</a></li>
    *)

    let navLinks currentBaseUri =
        div [Bootstrap.collapse; Bootstrap.navbar_collapse] [
            ul [Bootstrap.nav; Bootstrap.navbar_nav] [
                if auth.IsLoggedIn then
                    yield upcast navCommand "Log Out" auth.LogOut
                else
                    // Here we use a type-safe uri builder to that ensures our link is correct
                    // wherever we reference it.
                    yield upcast navLink "Log In" (Routes.Session.LogIn.CreateUri()) currentBaseUri
                    yield upcast navLink "Sign Up" (Routes.Session.SignUp.CreateUri()) currentBaseUri
            ]
        ]

    member __.Apply currentBaseUri title blurb children =
        let navLinks = navLinks currentBaseUri

        body [] [
            nav [Bootstrap.navbar; Bootstrap.navbar_default; Bootstrap.navbar_fixed_top; Bootstrap.navbar_inverse] [
                div [Bootstrap.container_fluid] [
                    div [Bootstrap.navbar_header] [
                        navbarToggleButton navLinks
                        a [Bootstrap.navbar_brand] [] |> Element.appendText ["HashBang Chat"]
                    ]
                    navLinks
                ]
            ] |> Unchecked.set "role" "navigation"

            div [Bootstrap.container_fluid] [
                yield h1 [] [] --> title :> _

                yield p [Bootstrap.lead] [] --> blurb :> _

                yield alertsSection() :> _

                yield! children
            ]
                    
            div [Application.footer] [
                div [Bootstrap.container_fluid][
                    p [Bootstrap.text_center] [] --> "Copyright 2014 Type Inferred Ltd."
                ]
            ]
        ]