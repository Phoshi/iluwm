module IPC.NamedPipeIPCServer

open System.IO
open System.IO.Pipes
open System.Security.AccessControl
open System.Security.Principal
open System.Threading.Tasks
open Integration
open Logume
open Twime
open Microsoft.FSharpLu.Json

let listWindows (eventRunner: EventRunner.T) =
    let _formatWindow (node: LayoutTree.T) =
            node
    eventRunner.Tree
    |> TwimeRoot.displays
    |> List.collect Display.tags
    |> List.map Tag.layout
    |> List.collect LayoutTree.windowNodes
    |> List.map _formatWindow
    |> Compact.serialize
    
let listMarks (eventRunner: EventRunner.T) =
    let _formatWindow (node: LayoutTree.T) =
            LayoutTree.windowDefinition node
            |> Option.map (fun d -> d.Definition.marks)
            |> Option.map (fun marks -> List.map (fun ma -> (ma, node)) marks)
    eventRunner.Tree
    |> Tree.windowNodes
    |> List.map _formatWindow
    |> List.collect Option.toList
    |> List.collect id
    |> Compact.serialize
    
let listTags (eventRunner: EventRunner.T) =
    let _formatTag (tag: Tag.T) =
            (tag.Reference, tag.Meta)
    eventRunner.Tree
    |> TwimeRoot.displays
    |> List.collect Display.tags
    |> List.map _formatTag
    |> Compact.serialize

let handleIncomingCommand hotkeys callback line =
    let (command, args) = Parse.parse line
    
    let updater =
        (CommandBindings.bindingFor (ConfigurationBindings.configuredBindings hotkeys) command)
        |> Option.map CommandBinding.exec
        |> Option.map (fun b -> b args)
        |> Option.defaultValue (fun _ -> None)
        
    callback updater
    
    "done"
    
let handleIncomingQuery hotkeys (nodeLister: unit -> string) (tagLister: unit -> string) (markLister: unit -> string) line =
    let _hookAction hk =
        match (ConfigurationBindings.action hk) with
        | Integration.HotkeyAction.MinorMode mode -> Some ("minor", mode)
        | _ -> None
    let _formatHotkey hk =
        (ConfigurationBindings.modifiers hk, ConfigurationBindings.hotkey hk, ConfigurationBindings.name hk, _hookAction hk, ConfigurationBindings.mode hk)
    let stringify hks =
        Microsoft.FSharpLu.Json.Compact.serialize hks
        
        
    match line with
    | "hotkeys" ->
        ConfigurationBindings.configuredBindings hotkeys
        |> List.map _formatHotkey
        |> stringify
    | "windows" ->
        nodeLister ()
    | "tags" ->
        tagLister ()
    | "marks" ->
        markLister ()
    | _ -> "Hva?"

let runIpcServer (log: Log -> unit) (pipe: string) (handler: string -> string): Task =
    let _runIpcServer () =
        let permissivePipeSecurity =
                let ps = PipeSecurity()
                let id = SecurityIdentifier(WellKnownSidType.AuthenticatedUserSid, null)
                ps.SetAccessRule(PipeAccessRule(id, PipeAccessRights.ReadWrite, AccessControlType.Allow))
                
                ps
        use server =
            NamedPipeServerStreamAcl.Create(
                pipe,
                PipeDirection.InOut,
                1,
                PipeTransmissionMode.Message,
                PipeOptions.Asynchronous,
                0,
                0,
                permissivePipeSecurity)
        
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
        