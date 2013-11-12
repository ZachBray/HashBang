// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#r @"bin\Debug\TypeInferred.HashBang.Core.dll"
#r @"bin\Debug\TypeInferred.HashBang.Routing.dll"
open TypeInferred.HashBang



type Routes = ClientRoutesProvider< "
/messages                          # Messages.Send   # Broadcasts a new message
/messages?since=string_option      # Messages.Since  # Returns all the messages since the given id
">


Routes.Messages.Since.CreateHandler(fun ps -> ps.since)