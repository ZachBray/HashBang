/// This module contains types that the server uses to describe its state to the client.
[<FunScript.JS>]
module Chat.Domain.Query

open Chat.Domain.Identifiers

/// Represents a user of the chat system
type User =
    {
        Id : UserId
        FirstName : string
        SecondName : string
        Email : string // TODO: Don't expose email addresses. Show usernames instead.
    }

/// Represents a message in a conversation
type Message =
    {
        CorrelationId : MessageId
        Sender : User // TODO: user UserId and cache users on the client instead.
        Time : string
        EscapedContent : string
    }

/// Represents a conversation between multiple users
type Conversation =
    {
        CorrelationId : ConversationId
        Topic : string
        Participants : UserId[]
    }

type Result<'T> =
    | Success of 'T
    | Failure of string