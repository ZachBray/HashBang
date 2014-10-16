/// This module contains types that are used to instruct the server.
[<FunScript.JS>]
module Chat.Domain.Command

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
        Topic : string
        Invitees : Query.UserId[]
    }