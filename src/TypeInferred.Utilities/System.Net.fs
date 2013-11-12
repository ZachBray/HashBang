namespace System.Net

open System
open System.Diagnostics

module Netsh =
    let addUrlAcl address =
        let args = sprintf "http add urlacl url=%s user=%s\%s" address Environment.UserDomainName Environment.UserName
        let psi = 
            ProcessStartInfo(
                "netsh", args,
                Verb = "runas",
                CreateNoWindow = true,
                WindowStyle = ProcessWindowStyle.Hidden,
                UseShellExecute = true)
        Process.Start(psi).WaitForExit()

module HttpListener =

    let getContextAsync (listener : HttpListener) =
        Async.FromBeginEnd(listener.BeginGetContext, listener.EndGetContext)

    let create prefix handle =
        async {
            use listener = new HttpListener()
            listener.Prefixes.Add prefix
            listener.Start()
            while true do
                let! context = listener |> getContextAsync
                Async.Start (handle context.Request context.Response)
        } |> Async.StartDisposable


[<ReflectedDefinition>]
module WebUtility =
    
    module Helpers =
        let rec consumeParam (query : string) i acc =
            match query.IndexOf('=', i) with
            | -1 -> acc//failwith "Invalid query string."
            | j -> consumeParamValue query (j+1) (query.Substring(i, j - i)) acc

        and consumeParamValue (query : string) i name acc =
            match query.IndexOf('&', i) with
            | -1 -> acc |> Map.add name (WebUtility.UrlDecode(query.Substring(i, query.Length - i)))
            | j -> consumeParam query (j+1) (acc |> Map.add name (WebUtility.UrlDecode((query.Substring(i, j - i)))))

        let consumeParams query = consumeParam query 0 Map.empty
        
    let SplitRelativeUri (uri : string) =
        let parts = uri.Split([|'?'|], StringSplitOptions.RemoveEmptyEntries)
        let segments =
            if parts.Length = 1 || parts.Length = 2 then
                 parts.[0].Split([|'/'|], StringSplitOptions.RemoveEmptyEntries)
            else failwith "Multiple '?' characters found in URI."
        let queryParameters =
            if parts.Length = 2 then
                Helpers.consumeParams parts.[1]
            else Map.empty
        segments, queryParameters
        
    let CreateUri (baseUri : string, segments, queryParameters : Map<_,_>) =
        let segments =
            let sep = if baseUri.Length = 0 || baseUri.[baseUri.Length - 1] <> '/' then "/" else ""
            baseUri + sep + (segments |> String.concat "/")
        let queryString =
            if queryParameters.Count = 0 then ""
            else
                let queryParams =
                    queryParameters |> Map.toArray |> Array.map (fun (name, value) ->
                        name + "=" + WebUtility.UrlEncode value)
                    |> String.concat "&"
                "?" + queryParams
        segments + queryString