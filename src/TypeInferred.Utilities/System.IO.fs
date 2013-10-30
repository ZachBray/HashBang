namespace System.IO

open System
open System.Net
open System.Net.Security

type TempFile() =
    let path = Path.GetTempFileName()

    member __.Path = path

    interface IDisposable with
        member __.Dispose() = File.Delete path

module Path =
    let CreateTempFile() = new TempFile()


module File =

    /// Open a file from file system or from the web in a type provider context
    /// (File may be relative to the type provider resolution folder and web
    /// resources must start with 'http://' prefix)
    let OpenStreamRelativeAbsoluteOrHttp(resolutionFolder, fileName:string) =
        if fileName.StartsWith("http://", StringComparison.InvariantCultureIgnoreCase) ||
            fileName.StartsWith("https://", StringComparison.InvariantCultureIgnoreCase) then
            let acceptAllCerts = RemoteCertificateValidationCallback(fun _ _ _ _ -> true)
            ServicePointManager.ServerCertificateValidationCallback <- acceptAllCerts            
            let req = System.Net.WebRequest.Create(Uri(fileName))
            let resp = req.GetResponse() 
            new StreamReader(resp.GetResponseStream())
        else
            // If the second path is absolute, Path.Combine returns it without change
            let file = 
                if fileName.StartsWith ".." then Path.Combine(resolutionFolder, fileName)
                else fileName
            new StreamReader(file)

    let ReadAllTextRelativeAbsoluteOrHttpText(resolutionFolder, fileName) =
        use stream = OpenStreamRelativeAbsoluteOrHttp(resolutionFolder, fileName)
        stream.ReadToEnd()