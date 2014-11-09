namespace Chat.Client.ViewModels

open System
open FunScript
open FunScript.TypeScript
open FunScript.Rx
open Chat.Domain.Identifiers
open Chat.Domain.Query
open Chat.Client
open Chat.Server
open Chat.Client.ViewModels

[<JS>]
type SenderType =
    | LoggedInUser
    | OtherUser

[<JS>]
type ConversationViewModel
        (alerts : AlertsViewModel,
         authViewModel : AuthenticationViewModel,
         userService : IUserService,
         conversationService : IConversationService) =

    let computeSenderType (user : User) (message : Message) =
        if message.Sender.Id = user.Id then LoggedInUser
        else OtherUser

    member __.FindUser email = userService.FindUser email

    member __.FindUsers(nameFilter : string) =
        async {
            if nameFilter.Length < 3 then return [||]
            else return! userService.FindUsers nameFilter
        }

    member __.ProposeConversation proposal =
        async {
            let! _, token = authViewModel.GetAccessTokenAsync()
            let! result = conversationService.Propose(token, proposal)
            // TODO: Add to conversations here (in pending state)
            match result with
            | Failure error -> alerts.Show(Danger, "Failed to create conversation", error)
            | Success conversation -> ()
        }

    member __.GetConversationsSafe() =
        authViewModel.GetAccessTokenObservable()
        |> Observable.map (snd >> conversationService.GetConversations)
        |> Observable.switch
        |> Observable.catchWith (fun ex ->
            alerts.Show(Danger, "Conversation stream error", ex.Message)
            Observable.empty
        )

    member __.GetMessagesSafe refId =
        Observable.defer(fun () ->
            try 
                let conversationId = ConversationId.Parse refId
                authViewModel.GetAccessTokenObservable()
                |> Observable.map (fun (loggedInUser, accessToken) ->
                    conversationService.GetMessages(accessToken, conversationId)
                    |> Observable.map (fun message -> computeSenderType loggedInUser message, message))
                |> Observable.switch
                |> Observable.catchWith (fun ex ->
                    alerts.Show(Danger, "Messages stream error", ex.Message)
                    Observable.empty
                )
            with ex ->
                alerts.Show(Danger, "Messages stream error", "Conversation id is invalid.")
                Observable.empty
        )