module IPC.NamedPipeIPCServer

open System.IO
open System.IO.Pipes
open System.Threading.Tasks
open Integration
open Logume

let handleIncomingCommand hotkeys callback line =
    let (command, args) = Parse.parse line
    
    let updater =
        (CommandBindings.bindingFor (ConfigurationBindings.configuredBindings hotkeys) command)
        |> Option.map CommandBinding.binding
        |> Option.map (fun b -> b args)
        |> Option.defaultValue (fun _ -> None)
        
    callback updater
    
    "done"
    
let handleIncomingQuery hotkeys line =
    let _formatHotkey hk =
        (ConfigurationBindings.modifiers hk, ConfigurationBindings.hotkey hk, ConfigurationBindings.name hk)
    let stringify hks =
        Microsoft.FSharpLu.Json.Compact.serialize hks
        
        
    match line with
    | "hotkeys" ->
        ConfigurationBindings.configuredBindings hotkeys
        |> List.map _formatHotkey
        |> stringify
    | _ -> "Hva?"

let runIpcServer (log: Log -> unit) (pipe: string) (handler: string -> string): Task =
    let _runIpcServer () =
        use server = new NamedPipeServerStream(pipe)
        while true do
            try
                Message.message "Waiting for new IPC connection"
                |> Message.trivial
                |> log
                
                server.WaitForConnection()
                
                Message.message "Connected to new IPC client"
                |> Message.trivial
                |> log
                
                let reader = new StreamReader(server)
                let writer = new StreamWriter(server)
                
                let line = reader.ReadLine()
                
                Message.message ("New IPC command: " + line)
                |> Message.addGauge "command" line
                |> Message.trivial
                |> log
                
                let oneline (lines: string) =
                    lines.Replace("\r\n", "")
                    
                let output = 
                    handler line
                    |> oneline
                
                writer.WriteLine(output)
                writer.Flush()
                server.Disconnect()
                
                Message.message "IPC disconnected"
                |> Message.trivial
                |> log
                
                Message.message "All done"
                |> Message.trivial
                |> log
            with
            | _ -> 
                Message.message "IPC error"
                |> Message.trivial
                |> log
            
            
        Message.message "Too done"
        |> Message.warning
        |> log
            
                
    Task.Factory.StartNew(_runIpcServer)
        