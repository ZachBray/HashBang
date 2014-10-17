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
    Head.empty ==> [
        yield upcast (Title.empty |> Element.appendText ["Chat Sample - HashBang"])

        yield upcast (Meta.empty |> Meta.name (Name.Generator "viewport")
                      |> Meta.content "width=device-width, initial-scale=1.0")

        for sheet in Stylesheets.all do
            yield Style.empty |> Style.``type`` IStyleElementType.Text_css
                  |> Element.appendText [ sheet ] :> _
    ]
    |> Element.appendText [ie8MediaFix]

let headedPage(body:HtmlTag<IBodyElement>, script:HtmlTag<IScriptElement>) =
    Html.empty ==> [
        yield upcast head
        yield upcast body
        for s in Scripts.all do
            yield upcast (Script.empty |> Element.appendText ["// <![CDATA[\n"; s; "\n// ]]>"] )
        yield upcast script
    ]

let errorTemplate(error:string) =
    Body.empty ==> [
        H1.empty --> "Error"
        P.empty --> error
    ]

let navLink description uri currentUri =
    Li.empty +. [if uri = currentUri then yield Bootstrap.active] ==> [
        (A.empty |> A.href uri ) --> description
    ]

let insideNavBar currentBaseUri title blurb children =
    Body.empty ==> [
        !![Application.site_wrapper] ==> [
            !![Application.site_wrapper_inner] ==> [
                !![Application.cover_container] ==> [

                    !![Application.masthead; Bootstrap.clearfix] ==> [
                        !![Application.inner] ==> [
                            H3.empty +. [Application.masthead_brand] --> "Chat"
                            !![Bootstrap.nav; Application.masthead_nav] ==> [
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
                    !![Application.inner; Application.cover] ==> [
                        H1.empty --> title

                        P.empty +. [Bootstrap.lead] --> blurb

                        Div.empty ==> children
                    ]
                    
                    !![Application.mastfoot] ==> [
                        !![Application.inner] ==> [
                            P.empty --> "Copyright 2014 Type Inferred Ltd."
                        ]
                    ]
                ]
            ]
        ]
    ]