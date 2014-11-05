/// This module contains types that are used to instruct the server.
[<FunScript.JS>]
module Chat.Domain.Command

open Chat.Domain.Identifiers

/// Represents the users details from the sign-up page
type UserDetails =
    {
        FirstName : string
        SecondName : string
        Email : string
        Password : string
    }

/// Represents a proposal to start a conversation with a group of users
type ConversationProposal =
    {
        CorrelationId : ConversationId
        Topic : string
        Invitees : UserId[]
    }

/// Represents a message
type MessageDetails =
    {
        CorrelationId : MessageId
        UnescapedContent : string
    }