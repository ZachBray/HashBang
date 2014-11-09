/// This module contains types that the server uses to describe its state to the client.
[<FunScript.JS>]
module Chat.Domain.Identifiers

open System

type UserId = 
    | UserId of Guid
    member userId.AsParameter = let (UserId id) = userId in id.ToString()
    static member NewRandom() = UserId(Guid.NewGuid())

type ConversationId = 
    | ConversationId of Guid
    member convId.AsParameter = let (ConversationId id) = convId in id.ToString()
    static member Parse str = ConversationId(Guid.Parse str)
    static member NewRandom() = ConversationId(Guid.NewGuid())
    

type MessageId = 
    | MessageId of Guid
    static member NewRandom() = MessageId(Guid.NewGuid())

/// Provided by the server on log in. Allows access to secured methods.
type AccessToken = 
    | AccessToken of Guid
    static member NewRandom() = AccessToken(Guid.NewGuid())