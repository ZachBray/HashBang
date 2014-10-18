[<FunScript.JS>]
module Chat.Client.Templates

open TypeInferred.HashBang.Html
open TypeInferred.HashBang.Html.Extensions
open Chat.Client
open Chat.Client.Stylesheets

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
        h1 [] [] --> "Error"
        p [] [] --> error
    ]

let navLink description uri currentUri =
    li [if uri = currentUri then yield Bootstrap.active] [
        a [] [] --> description |> A.href uri
    ]

let insideNavBar currentBaseUri title blurb children =
    body [] [
        div [Application.site_wrapper] [
            div [Application.site_wrapper_inner] [
                div [Application.cover_container] [

                    div [Application.masthead; Bootstrap.clearfix] [
                        div [Application.inner] [
                            h3 [Application.masthead_brand] [] --> "Chat"
                            div [Bootstrap.nav; Application.masthead_nav] [
                                // Here we use a type-safe uri builder to that ensures our link is correct
                                // wherever we reference it.
                                navLink "Log In" (Routes.Session.LogIn.CreateUri()) currentBaseUri
                                navLink "Sign Up" (Routes.Session.SignUp.CreateUri()) currentBaseUri
                            ]
                        ]
                    ]

                    // Note: we took this template from: http://getbootstrap.com/examples/cover/#
                    //       and interestingly we found a "mistake" here using the type provider.
                    //       They reference a class cover-heading that isn't used.
                    div [Application.inner; Application.cover] [
                        h1 [] [] --> title

                        p [Bootstrap.lead] [] --> blurb

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