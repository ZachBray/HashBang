namespace TypeInferred.HashBang

open System.Net

open TypeInferred.HashBang.Runtime

module Authorization =
    let tryGetToken (req : HttpListenerRequest) =
        match req.Headers.GetValues "Authorization" with
            | [|auth|] -> 
                match auth.Split [|' '|] with
                | [| "HashBang-Auth"; token |] -> Some token
                | _ -> None
            | _ -> None

    let filterByAuthToken (authTokens : TimedCache<_,_>) handler =
        handler |> RequestHandler.bindIn (fun (Request(_,_,req : HttpListenerRequest)) () -> async {
            match tryGetToken req with
            | None -> return Unauthorized
            | Some token ->
                match authTokens.TryFind token with
                | Some user -> return OK(req, user)
                | None -> return Unauthorized
        })
        |> RequestHandler.addHeaderMetadata {
            Key = "HashBang-Auth"
            Type = StringType
            IsRequired = true
        }