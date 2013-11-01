[<ReflectedDefinition>]
module Program

// Here we open up some namespaces.
open FunScript
open FSharp.Data
open System.IO
open TypeInferred.HashBang
open ChatExample.Shared

// Here we create some helpers for jQuery interop.
let jq(selector : string) = Globals.Dollar.Invoke selector
let (?) jq name = jq("#" + name)

// Here we open up the type-safe ChatServer API using 
// the HashBangAPI type provider. We pass it the address of the
// SchemaUrl so that it knows what the API looks like. This SchemaUrl
// can be a web address or file system address, which may be useful if you
// want to cache the schema on disk.
type ChatServer = HashBangAPI<SchemaUrl = "http://localhost:8080/metadata">

// Here we lazily open up a connection to the ChatServer API. We pass it
// the runtime base address.
let ctx = lazy ChatServer.GetDataContext("http://localhost:8080/")

// Here we continuously poll the ChatServer for new messages 
// using Comet (or long polling).
// See: http://en.wikipedia.org/wiki/Comet_(programming)
let rec trackMessages lastId =
    async {
        let getMessagesAsync =
            match lastId with
            // We need to get all the messages if we haven't seen a message yet.
            | None -> ctx.Value.Message.GetAll()
            // Otherwise, we need to get all the messages since the last message.
            | Some id -> ctx.Value.Message.GetSince id
        // We ask the server for the messages
        let! messages = getMessagesAsync
        match messages with
        // If the server has no new messages we try again.
        | None -> return! trackMessages lastId
        | Some msgs ->
            match msgs.Length with
            | 0 -> return! trackMessages lastId
            // Otherwise, we add the messages to the screen.
            | _ ->
                // Note: the messages come in order: most recent -> least recent
                msgs |> Array.rev |> Array.iter (fun (_, msg) ->
                    jq?messages.append(
                        "<div class=\"alert alert-success\">" 
                        + msg.Content + "</div>") 
                    |> ignore)
                // Here we pull out the last message id that we've seen and poll
                // again.
                let nextId, _ = msgs.[0]
                return! trackMessages (Some nextId)
    }

// TASK: STEP 4
// ============
let sendMessage() =
    (* TODO: send message to the server *)
    (* Hints: (jq?element_name)._val() to get current value
              (jq?element_name)._val(newValue) to set value
    *)
    Globals.alert("Not implemented")

// Here we define the main function that will be compiled into
// JavaScript at root level.
let main () =
    // First, we set up the "send" button to send a message.
    (jq?submit).click(fun _ -> sendMessage()) |> ignore
    // Second, we set up the enter key to send a message.
    (jq?content).keydown(fun (keyArgs : JQueryKeyEventObject) ->
        if int keyArgs.keyCode = 13 then sendMessage()
        null) |> ignore
    // Third, we continuously long-poll the server for new
    // messages.
    trackMessages None |> Async.StartImmediate
    

// Here we compile the main() function into javascript code...
let code = Compiler.Compiler.Compile(<@ main() @>, noReturn=true)

// We write the compiled JS to a file...
let codeFile = Path.Combine(__SOURCE_DIRECTORY__, "www", "js", "app.js")
File.WriteAllText(codeFile, code)

// We open the index page in the default web browser...
let indexFile = Path.Combine(__SOURCE_DIRECTORY__, "www", "index.html")
System.Diagnostics.Process.Start(indexFile) |> ignore