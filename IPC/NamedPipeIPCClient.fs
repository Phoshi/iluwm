module IPC.NamedPipeIPCClient

open System
open System.IO
open System.IO.Pipes

let send pipe (message: string) =
    use client = new NamedPipeClientStream(pipe)
    
    use reader = new StreamReader(client)
    use writer = new StreamWriter(client)
    
    client.Connect()
    writer.WriteLine(message)
    writer.Flush()
    reader.ReadLine()
