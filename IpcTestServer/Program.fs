// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Threading.Tasks

// Define a function to construct a message to print

[<EntryPoint>]
let main argv =
    IPC.NamedPipeIPCServer.runIpcServer
        (Logume.Logger.logToConsole "ipc")
        "iluwmipc"
        ignore
    |> Task.WaitAll
        
    0 // return an integer exit code