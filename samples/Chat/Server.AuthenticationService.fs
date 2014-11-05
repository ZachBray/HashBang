namespace Chat.Server

open System
open System.Text
open System.Collections.Concurrent
open System.Security.Cryptography
open FSharp.Control.Reactive
open TypeInferred.HashBang
open TypeInferred.HashBang.SignalR
open Chat.Domain.Query
open Chat.Domain.Command
open Chat.Server.Utilities

type IAuthenticationService =
    inherit IService

    abstract IsEmailRegistered : email:string -> bool Async
    abstract SignUp : UserDetails -> AccessToken IObservable
    abstract LogIn : email:string * password:string -> AccessToken IObservable

/// This is a server-side service providing authentication.
/// We can use an interface here, which would make the page more testable.
type AuthenticationService() =
    
    let users = ConcurrentDictionary()
    let accessTokens = ConcurrentDictionary()
    let accessTokensChanged = Event<_>()

    let rec generateAccessToken user =
        let accessToken = AccessToken(Guid.NewGuid().ToString())
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
                Id = UserId(Guid.NewGuid().ToString())
                FirstName = details.FirstName
                SecondName = details.SecondName
                Email = details.Email
            }
            let salt = Cryptography.generateSalt()
            let saltedPassword = Cryptography.computeHash details.Password salt
            if users.TryAdd(newUser.Email, (newUser, salt, saltedPassword)) then
                __.LogIn(details.Email, details.Password)
            else Observable.throw(exn(sprintf "Email %s is already registered." details.Email))
        )

    /// Returns an access token in exchange for valid credentials. Otherwise it errors.
    /// Once the subscription is disposed the session ends and accessToken becomes invalid.
    /// Note: In a real system we would use an encrypted connection to avoid sending
    ///       the password insecurely across the wire.
    member __.LogIn(email, password) =
        Observable.defer(fun () ->
            match users.TryGetValue(email) with
            | false, _ -> Observable.throw(exn(sprintf "Cannot find a registered user with email: %s" email))
            | true, (user, salt, saltedPassword) ->
                let providedSaltedPassword = Cryptography.computeHash password salt
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

    
    interface IAuthenticationService with
        member __.IsEmailRegistered email = __.IsEmailRegistered email
        member __.LogIn(email, password) = __.LogIn(email, password)
        member __.SignUp userDetails = __.SignUp userDetails

    interface IDisposable with
        member __.Dispose() = 
            sha256.Dispose()
            rand.Dispose()