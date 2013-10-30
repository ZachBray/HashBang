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
            Netsh.addUrlAcl prefix
            use listener = new HttpListener()
            listener.Prefixes.Add prefix
            listener.Start()
            while true do
                let! context = listener |> getContextAsync
                Async.Start (handle context.Request context.Response)
        } |> Async.StartDisposable


