# The broken chat server task

* Prerequisites
    * Visual Studio with F# 3 and NuGet
    * Download the [Prog. F# session task zip file](www.funscript.info)
* Steps
    1. 
        * Build and run the ChatExample.Server project
        * Build and run the ChatExample.Client project
        * Verify that the send button doesn't work
    2.  
        * Stop the ChatExample.Server program
    3.  
        * Implement the POST "/messages" handler (`Send`) in server.fs
        * Build and run the ChatExample.Server project
        * Verify that the type provider context now shows a `Messages.Send` method
    4. 
        * Handle the send button click and mouse enter key in client.fs
        * Build and run the ChatExample.Client project
        * Verify that the send button works
* Hints at:
[https://github.com/ZachBray/HashBang/tree/master/src/Examples/Chat](https://github.com/ZachBray/HashBang/tree/master/src/Examples/Chat)
