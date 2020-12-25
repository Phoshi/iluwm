// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
open System.IO.Pipes
open IPC.NamedPipeIPCClient

let join (str: string[]) =
    String.Join(' ', str)

[<EntryPoint>]
let main argv =
    let response =
        send "iluwmipcquery"
            (argv
             |> join)
    Console.WriteLine response
    
    0