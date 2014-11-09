namespace Chat.Client.Pages

open System
open FunScript.TypeScript
open FunScript.Rx
open TypeInferred.HashBang
open TypeInferred.HashBang.Html
open TypeInferred.HashBang.Bootstrap
open Chat.Domain.Query
open Chat.Domain.Command
open Chat.Domain.Identifiers
open Chat.Domain
open Chat.Server
open Chat.Client
open Chat.Client.Stylesheets
open Chat.Client.Templates
open Chat.Client.ViewModels

[<FunScript.JS>]
type ConversationPage
        (navBarPageTemplate : NavBarPageTemplate,
         conversationViewModel : ConversationViewModel) =
    
    let baseUri = Routes.Conversation.View.CreateUri() 
    
    let isEmail = Validate.checkThat Validation.isEmail "Must be a valid email."
    
    let toUser emailFormQuery =
        emailFormQuery |> FormQuery.mapValueAsync (fun v ->
            async {
                match v with
                | Valid email -> 
                    let! userResult = conversationViewModel.FindUser email
                    match userResult with
                    | Success user -> return Valid user
                    | Failure error -> return Invalid(FieldError error)
                | Invalid error -> return Invalid error 
            })

    let startConversationForm closeModal =
        FormQuery.puree (fun (friend : User) topic -> 
        { 
            Topic = topic
            Invitees = [|friend.Id|]
            CorrelationId = ConversationId.NewRandom()
        })
        <*> Inputs.typeahead "Friend" "joebloggs@gmail.com" 
                conversationViewModel.FindUsers
                (fun user -> user.Email)
                (fun user -> p [] [] |> Element.appendText [user.FirstName + " " + user.SecondName + " | " + user.Email])
                (Validate.isNotEmpty >> isEmail >> toUser)
        <*> Inputs.text "Topic" "We need to talk about..." (Validate.isNotEmpty)
        |> FormQuery.withSubmitButton "Start Conversation" (fun proposal -> 
            async {
                do! conversationViewModel.ProposeConversation proposal
                closeModal()
            })
        |> FormQuery.withDisablingFieldset
        |> FormQuery.runBootstrap

    let startConversationDialog() =
        Modal.show {
            Modal.ModalDescription.TitledWithClose "Start Conversation" with
                BodyFactory = fun context -> [startConversationForm context.Close]
        }

    let formatConversation(conversation : Conversation) =
        let conversationIdParameter = Some(conversation.CorrelationId.AsParameter)
        div [Bootstrap.row] [
            div [Bootstrap.col_sm_12] [
                a [] [] |> Element.appendText [conversation.Topic]
                |> A.href (Routes.Conversation.View.CreateUri conversationIdParameter)
            ]
        ]

    let conversations() =
        div [] [] |> Element.appendSetUpByJQuery (fun query ->
            conversationViewModel.GetConversationsSafe()
            |> Observable.subscribe (fun conversation ->
                let conversationLink = formatConversation conversation
                let resources = query.append conversationLink
                resources.Dispose() // No resources to handle in links
            )
        )

    let formatMessage = function
        | LoggedInUser, (message : Message) ->
            div [Bootstrap.row] [
                div [Bootstrap.col_sm_9; Bootstrap.col_sm_offset_3] [
                    div [Application.message; Application.from_me] [
                        div [Bootstrap.col_sm_2] [] |> Element.appendText [message.Time]
                        div [Bootstrap.col_sm_2] [] |> Element.appendText [message.Sender.FirstName]
                        div [Bootstrap.col_sm_8] [] |> Element.appendText [message.EscapedContent]
                    ]
                ]
            ]
        | OtherUser, message ->
            div [Bootstrap.row] [
                div [Bootstrap.col_sm_9] [
                    div [Application.message; Application.from_friend] [
                        div [Bootstrap.col_sm_2] [] |> Element.appendText [message.Time]
                        div [Bootstrap.col_sm_2] [] |> Element.appendText [message.Sender.FirstName]
                        div [Bootstrap.col_sm_8] [] |> Element.appendText [message.EscapedContent]
                    ]
                ]
            ]

    let createMessages conversationId =
        div [] [] 
        |> Element.appendSetUpByJQuery (fun query ->
            match conversationId with
            | None -> 
                query.append "No messages." |> ignore
                Disposable.empty
            | Some id ->
                conversationViewModel.GetMessagesSafe(id)
                |> Observable.subscribe (fun message ->
                    let messageDiv = formatMessage message
                    let resources = query.append messageDiv
                    resources.Dispose() // No resources to handle in links
                )

            
        )

    let createPage conversationId =
        navBarPageTemplate.Apply
            baseUri
            "Conversation" 
            "Talk to people."
            [
                div [Bootstrap.row] [
                    div [Bootstrap.col_sm_3] [
                        h5 [] [] |> Element.appendText ["Conversations"]

                        conversations()
                        
                        a [] [] |> Element.appendText ["Start new conversation"]
                        |> A.href "#"
                        |> Element.onclick(fun evt -> 
                            evt.preventDefault()
                            startConversationDialog())
                    ]
                    div [Bootstrap.col_sm_9] [
                        h5 [] [] |> Element.appendText ["Messages"]
                        createMessages(conversationId)
                    ]
                ]
            ]
    
    interface IPage with
        member val RequestHandler = 
            Routes.Conversation.View.CreateHandler (fun ps ->
                async { return Some(createPage ps.id) }
            )