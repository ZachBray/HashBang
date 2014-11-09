namespace Chat.Server

open System
open System.Collections.Concurrent
open System.Reactive.Subjects
open FSharp.Control.Reactive
open TypeInferred.HashBang
open Chat.Domain.Identifiers
open Chat.Domain.Query
open Chat.Domain.Command

type IConversationService =
    inherit IService
    abstract Propose : AccessToken * ConversationProposal -> Conversation Result Async
    abstract Send : AccessToken * ConversationId * MessageDetails -> Message Result Async
    abstract GetConversations : AccessToken -> Conversation IObservable
    abstract GetMessages : AccessToken * ConversationId -> Message IObservable

type ConversationService(tokenExchange : IAccessTokenExchangeService) =
    
    let conversationLimit = 10
    let messageLimit = 50

    // TODO: Messages are never cleaned up...
    let conversationsById = ConcurrentDictionary()
    let conversationsByUserId = ConcurrentDictionary()

    let tryGetMessages conversationId =
        match conversationsById.TryGetValue conversationId with
        | false, _ -> None
        | true, conversations -> Some conversations

    let rec getConversations userId =
        match conversationsByUserId.TryGetValue userId with
        | false, _ ->
            conversationsByUserId.TryAdd(userId, new ReplaySubject<_>(conversationLimit)) |> ignore
            getConversations userId
        | true, conversations -> conversations

    let addConversation conversation userId =
        let conversations = getConversations userId
        conversations.OnNext conversation

    member __.Propose(accessToken, proposal : ConversationProposal) =
        async {
            match tokenExchange.TryExchangeTokenForUser accessToken with
            | None -> return Failure "Access token is invalid"
            | Some user ->
                let participants = 
                    [| yield user.Id; yield! proposal.Invitees|] 
                    |> Seq.distinct
                    |> Seq.toArray
                let conversation = {
                    CorrelationId = proposal.CorrelationId
                    Topic = Uri.EscapeDataString proposal.Topic
                    Participants = participants
                }
                if not (conversationsById.TryAdd(conversation.CorrelationId, new ReplaySubject<_>(messageLimit))) then 
                    failwith "Conversation id collision"
                participants |> Array.iter (addConversation conversation)
                return Success conversation
        }

    member __.Send(accessToken, conversationId, details : MessageDetails) =
        async {
            match tokenExchange.TryExchangeTokenForUser accessToken with
            | None -> return Failure "Access token is invalid"
            | Some user ->
                match tryGetMessages conversationId with
                | None -> return Failure "Conversation id is invalid"
                | Some messages ->
                    let message = {
                        CorrelationId = details.CorrelationId
                        Sender = user
                        Time = DateTimeOffset.UtcNow.ToString()
                        EscapedContent = System.Uri.EscapeDataString details.UnescapedContent
                    }
                    messages.OnNext message
                    return Success message
        }

    member __.GetConversations accessToken =
        Observable.defer(fun () ->
            match tokenExchange.TryExchangeTokenForUser accessToken with
            | None -> Observable.throw (exn "Access token is invalid")
            | Some user -> upcast getConversations user.Id
        )

    member __.GetMessages(accessToken, conversationId) =
        Observable.defer(fun () ->
            match tokenExchange.TryExchangeTokenForUser accessToken with
            | None -> Observable.throw (exn "Access token is invalid")
            | Some user -> 
                match tryGetMessages conversationId with
                | None -> Observable.throw (exn "Covnersation id is invalid")
                | Some messages -> upcast messages
        )

    interface IConversationService with
        member __.Propose(accessToken, proposal) = __.Propose(accessToken, proposal)
        member __.Send(accessToken, conversationId, details) = __.Send(accessToken, conversationId, details)
        member __.GetConversations accessToken = __.GetConversations accessToken
        member __.GetMessages(accessToken, conversationId) = __.GetMessages(accessToken, conversationId)