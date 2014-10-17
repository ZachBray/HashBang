namespace TypeInferred.HashBang.Logging

open System

type DetailLevel = Error | Warn | Info | Debug

type ILogAppender =
    abstract AppendLog: DetailLevel * DateTimeOffset * message:string -> unit

/// Appends logs to the console.
type ConsoleLogAppender() =
    interface ILogAppender with
        member __.AppendLog(detailLevel, time, content) = 
            System.Console.WriteLine(sprintf "[%A] %s : %s" detailLevel (time.ToString("u")) content)

/// A logger that inlines calls to avoid building strings for turned off log levels
type internal Logger(maximumDetailLevel, appenders : ILogAppender list) = 
    member val Appenders = appenders |> List.toArray
    member val MaximumDetailLevel = maximumDetailLevel
    member inline __.Log(detailLevel, contentFactory) =
        if detailLevel <= __.MaximumDetailLevel then
            let time = DateTimeOffset.UtcNow
            let content = contentFactory()
            __.Appenders |> Array.iter (fun appender -> 
                appender.AppendLog(detailLevel, time, content))