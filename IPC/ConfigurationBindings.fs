module IPC.ConfigurationBindings

open Integration
open Integration.HotkeyAction
open Integration.Hotkey
open Twime

type T = {
        hotkey: Keystroke.T
        modifiers: Keystroke.T list
        name: string
        action: TwimeRoot.Update
}

let name t = t.name
let hotkey t = t.hotkey
let modifiers t = t.modifiers
let action t = t.action

let create name key mods action =
    {
        name = name
        hotkey = key
        modifiers = mods
        action = action
    }

let configuredBindings (hotkeys: HotkeyAction.T list) =
    let nameFor h =
        let join (strs: string list) =
            System.String.Join('_', strs)
            
        HotkeyAction.keys h
        |> List.map (fun k -> k.ToString())
        |> join
            
            
    let _binding h =
        create
            (nameFor h)
            (HotkeyAction.key h)
            (HotkeyAction.modifiers h)
            (HotkeyAction.action h)
        
        
    hotkeys 
    |> List.map _binding
    
module Tests =
    open FsUnit
    open NUnit.Framework
    
    [<TestFixture>]
    type ``Given a list of hotkey bindings`` () =
            let mutable _called = None
            let nop c _ =
                _called <- Some c
                None
            let hotkeys = [
                Windows ^+ A := nop "a"
                Windows ^+ B := nop "b"
            ]
            
            [<Test>]
            member x.``there are two computed bindings`` () =
                    configuredBindings hotkeys
                    |> List.map name
                    |> should equivalent ["LWin_A"; "LWin_B"]
                    
            [<Test>]
            member a.``LWin A has the correct key configuration`` () =
                    let lwinA =
                        configuredBindings hotkeys
                        |> List.find (fun h -> name h = "LWin_A")
                        
                    lwinA
                    |> hotkey
                    |> should equal Keystroke.T.A
                    
                    lwinA
                    |> modifiers
                    |> should equivalent [Keystroke.T.LWin]
                    
            [<Test>]
            member a.``LWin A executes the correct action`` () =
                    let lwinA =
                        configuredBindings hotkeys
                        |> List.find (fun h -> name h = "LWin_A")
                        
                    lwinA
                    |> action
                    <| TwimeRoot.create [] []
                    |> ignore
                    
                    _called |> should equal (Some "a")
                    