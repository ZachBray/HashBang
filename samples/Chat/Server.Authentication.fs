namespace Chat.Server

open System
open System.Text
open System.Collections.Concurrent
open System.Security.Cryptography
open FSharp.Control.Reactive
open TypeInferred.HashBang
open TypeInferred.HashBang.SignalR
open Chat.Domain.Identifiers
open Chat.Domain.Query
open Chat.Domain.Command

type IUserService =
    inherit IService
    abstract FindUser : email:string -> User Result Async
    abstract FindUsers : nameFilter:string -> User[] Async

type IAuthenticationService =
    inherit IService

    abstract IsEmailRegistered : email:string -> bool Async
    abstract SignUp : UserDetails -> AccessToken IObservable
    abstract LogIn : email:string * password:string -> AccessToken IObservable

type IAccessTokenExchangeService =
    abstract TryExchangeTokenForUser : AccessToken -> User option

/// This is a server-side service providing authentication.
/// We can use an interface here, which would make the page more testable.
type AuthenticationService() =
    
    let users = ConcurrentDictionary()
    let accessTokens = ConcurrentDictionary()
    let accessTokensChanged = Event<_>()

    let rec generateAccessToken user =
        let accessToken = AccessToken.NewRandom()
        if accessTokens.TryAdd(accessToken, user) then 
            accessTokensChanged.Trigger()
            accessToken
        else generateAccessToken user

    let sha256 = new SHA256Managed()
    let rand = new RNGCryptoServiceProvider()

    let randBytes n =
        let xs = Array.zeroCreate n
        rand.GetBytes xs
        xs

    let generateSalt() =
        randBytes 32 |> Convert.ToBase64String

    let computeHash (password : string) salt =
        let passBytes = password |> Encoding.UTF8.GetBytes
        let saltBytes = salt |> Convert.FromBase64String
        let allBytes = Array.append passBytes saltBytes
        sha256.ComputeHash allBytes |> Convert.ToBase64String


    /// Returns whether or not the email address provided has already been taken.
    member __.IsEmailRegistered(email) =
        // The code inside this method (and some others) isn't asynchronous!
        // However, it is called from the client (asynchronously) and using 
        // an Async return type here allows the client to call this method
        // as it would if the network wasn't there.
        async { return users.ContainsKey email }

    /// Returns the newly created user if the email address has not been taken.
    /// Otherwise it throws.
    member __.SignUp(details : UserDetails) =
        Observable.defer(fun () ->
            let newUser = {
                Id = UserId.NewRandom()
                FirstName = System.Uri.EscapeDataString details.FirstName
                SecondName = System.Uri.EscapeDataString details.SecondName
                Email = System.Uri.EscapeDataString details.Email
            }
            let salt = generateSalt()
            let saltedPassword = computeHash details.Password salt
            if users.TryAdd(newUser.Email, (newUser, salt, saltedPassword)) then
                __.LogIn(details.Email, details.Password)
            else Observable.throw(exn(sprintf "Email %s is already registered." newUser.Email))
        )

    /// Returns an access token in exchange for valid credentials. Otherwise it errors.
    /// Once the subscription is disposed the session ends and accessToken becomes invalid.
    /// Note: In a real system we would use an encrypted connection to avoid sending
    ///       the password insecurely across the wire.
    member __.LogIn(email, password) =
        Observable.defer(fun () ->
            let email = System.Uri.EscapeDataString email
            match users.TryGetValue(email) with
            | false, _ -> Observable.throw(exn(sprintf "Cannot find a registered user with email: %s" email))
            | true, (user, salt, saltedPassword) ->
                let providedSaltedPassword = computeHash password salt
                let areCredentialsValid = saltedPassword = providedSaltedPassword
                if not areCredentialsValid then Observable.throw(exn "Invalid password")
                else Observable.createWithDisposable(fun observer ->
                    let user, _, _ = users.[email]
                    let accessToken = generateAccessToken user
                    observer.OnNext accessToken
                    DisposableEx.create(fun () ->
                        accessTokens.TryRemove(accessToken) |> ignore
                        accessTokensChanged.Trigger()
                    )
                )
        )

    /// Returns the user with the provided email if any exist.
    member __.FindUser email =
        async {
            let email = System.Uri.EscapeDataString email
            match users.TryGetValue email with
            | false, _ -> return Failure ("No users exist with the email: " + System.Uri.EscapeDataString email)
            | true, (user,_,_) -> return Success user
        }

    /// If nameFilter has at least 3 characters this returns the users that 
    /// have a first or second name that matches the nameFilter.
    /// Otherwise, it returns empty.
    member __.FindUsers nameFilter =
        async {
            if nameFilter |> String.length < 3 then return [||]
            else
                // In a real application we'd probably use an index here.
                let allUsers = users.Values
                return allUsers |> Seq.choose (fun (user, _, _) ->
                    let isMatch = 
                        user.FirstName.StartsWith nameFilter 
                        || user.SecondName.StartsWith nameFilter
                    if isMatch then Some user
                    else None
                ) |> Seq.toArray
        }

    /// Returns the user associated with the provided access token if any exist.
    member __.TryExchangeTokenForUser accessToken =
        match accessTokens.TryGetValue accessToken with
        | false, _ -> None
        | true, user -> Some user
    
    interface IAuthenticationService with
        member __.IsEmailRegistered email = __.IsEmailRegistered email
        member __.LogIn(email, password) = __.LogIn(email, password)
        member __.SignUp userDetails = __.SignUp userDetails

    interface IUserService with
        member __.FindUser email = __.FindUser email
        member __.FindUsers nameFilter = __.FindUsers nameFilter

    interface IAccessTokenExchangeService with
        member __.TryExchangeTokenForUser accessToken = __.TryExchangeTokenForUser accessToken
    
    interface IDisposable with
        member __.Dispose() = 
            sha256.Dispose()
            rand.Dispose()