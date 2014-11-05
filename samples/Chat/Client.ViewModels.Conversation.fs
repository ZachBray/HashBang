namespace Chat.Client.ViewModels

open System
open FunScript
open FunScript.TypeScript
open FunScript.Rx
open Chat.Domain.Query
open Chat.Client
open Chat.Server
open Chat.Client.ViewModels

[<JS>]
type AuthenticationViewModel(alerts : AlertsViewModel, userService : IUserService, conversationService : IConversationService) =
    
    member __.FindUsers(nameFilter : string) =
        async {
            if nameFilter.Length < 3 then return [||]
            else return! userService.FindUsers nameFilter
        }