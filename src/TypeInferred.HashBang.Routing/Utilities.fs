[<AutoOpen>]
module internal TypeInferred.HashBang.Routing.Utilities

open System
open System.IO
open System.Net
open System.Net.Security
open System.Collections.Concurrent

/// Open a file from file system or from the web in a type provider context
/// (File may be relative to the type provider resolution folder and web
/// resources must start with 'http://' prefix)
let openStreamRelativeAbsoluteOrHttp(resolutionFolder, fileName:string) =
    if fileName.StartsWith("http://", StringComparison.InvariantCultureIgnoreCase) ||
        fileName.StartsWith("https://", StringComparison.InvariantCultureIgnoreCase) then
        let acceptAllCerts = RemoteCertificateValidationCallback(fun _ _ _ _ -> true)
        ServicePointManager.ServerCertificateValidationCallback <- acceptAllCerts            
        let req = System.Net.WebRequest.Create(Uri(fileName))
        let resp = req.GetResponse() 
        resp.GetResponseStream()
    else
        // If the second path is absolute, Path.Combine returns it without change
        let file = 
            if fileName.StartsWith ".." then Path.Combine(resolutionFolder, fileName)
            else fileName
        upcast new FileStream(file, FileMode.Open)

let readAllTextRelativeAbsoluteOrHttp(resolutionFolder, fileName) =
    use stream = openStreamRelativeAbsoluteOrHttp(resolutionFolder, fileName)
    use reader = new StreamReader(stream)
    reader.ReadToEnd()

let readAllBytesRelativeAbsoluteOrHttp(resolutionFolder, fileName) =
    use stream = openStreamRelativeAbsoluteOrHttp(resolutionFolder, fileName)
    use memory = new MemoryStream()
    stream.CopyTo memory
    stream.Flush()
    stream.Close()
    memory.ToArray()

let memoize f  =
    let cache = ConcurrentDictionary()
    let factory = Func<_,_>(fun x -> f x)
    fun x -> cache.GetOrAdd(key = x, valueFactory = factory)