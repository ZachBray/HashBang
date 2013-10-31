open System
open TypeInferred.HashBang
open ChatExample.Shared

type MessageBoxCommand =
    | Send of Message
    | GetSince of Guid option * AsyncReplyChannel<(string * Message)[]>

module List =
    let tryHead = function
        | [] -> None
        | x::_ -> Some x

let messageBox =
    MailboxProcessor.Start(fun inbox ->
        let notify msgs (lastId, channel : AsyncReplyChannel<_>) =
            msgs |> Seq.takeWhile (fun (id, _) -> 
                match lastId with
                | None -> true
                | Some lastId -> id <> lastId)
            |> Seq.map (fun (id, msg) -> id.ToString(), msg)
            |> Seq.toArray |> channel.Reply
        let rec loop msgs waiting =
            async {
                let! command = inbox.Receive()
                match command with
                | GetSince(lastId, channel) ->
                    match List.tryHead msgs with
                    | Some(id, _) when Some id <> lastId ->
                        notify msgs (lastId, channel)
                        return! loop msgs waiting
                    | Some _ | None ->
                        return! loop msgs ((lastId, channel) :: waiting)
                | Send msg ->
                    let id = Guid.NewGuid()
                    let newMsgs = (id, msg) :: msgs
                    waiting |> List.iter (notify newMsgs)
                    return! loop newMsgs []
            }
        loop [] []
    )

let createServer() =
    Website.At "http://*:8080/"
    |> Website.WithRouteHandlers 
        [
            Json.POST "/messages" { 
                Resource = "Message"; Action = "Send"
                Description = "Broadcasts a new message."
                Handle = fun (message : Message, ()) () -> async {
                    messageBox.Post(Send message)
                    return OK message
                }
            }

            Json.GET "/messages/all" {
                Resource = "Message"; Action = "GetAll"
                Description = "Returns all the messages so far."
                Handle = fun () () -> async {
                    let timeout = 10000
                    let! msgs = messageBox.PostAndTryAsyncReply((fun c -> GetSince(None, c)), timeout)
                    return OK msgs
                }
            }

            Json.GET "/messages/since/{id:%s}" {
                Resource = "Message"; Action = "GetSince"
                Description = "Returns all the messages since the given id."
                Handle = fun () lastId -> async {
                    let! msgs = messageBox.PostAndTryAsyncReply(fun c -> 
                                    GetSince(Some(Guid.Parse lastId), c))
                    return OK msgs
                }
            }
        ] 
    |> Website.WithMetadata
    |> Website.Start

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    use webserver = createServer()
    printfn "Press Return key to kill server."
    System.Console.ReadLine() |> ignore
    0 // return an integer exit code