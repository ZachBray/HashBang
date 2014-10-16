/// This module contains types that the server uses to describe its state to the client.
[<FunScript.JS>]
module Chat.Domain.Query

type UserId = UserId of string

/// Represents a user of the chat system
type User =
    {
        Id : UserId
        FirstName : string
        SecondName : string
        Email : string
    }

/// Represents a message in a conversation
type Message =
    {
        Sender : UserId
        Time : string
        EscapedContent : string
    }

type ConversationId = ConversationId of string

/// Represents a conversation between multiple users
type Conversation =
    {
        Id : ConversationId
        Topic : string
        Participants : UserId[]
    }

/// Provided by the server on log in. Allows access to secured methods.
type AccessToken = AccessToken of string