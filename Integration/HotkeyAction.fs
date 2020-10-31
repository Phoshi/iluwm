namespace Integration

open NUnit.Framework

module HotkeyAction =
    type T = {
        key: Keystroke.T
        modifiers: Keystroke.T list
        action: Twime.TwimeRoot.Update
    }
    
    let inline (:=) keys action : T =
        let keystrokes = Hotkey.toKeyStroke keys
        {
            key = keystrokes
                  |> List.last
            modifiers = keystrokes
                        |> List.rev
                        |> List.tail
                        |> List.rev
            action = action
        }
        
    let key t = t.key
    let modifiers t = t.modifiers
    let keys t = t.modifiers @ [t.key]
    let action t = t.action
    
    type Match = NoMatch | Partial | Full
    let private allPressed testKeys pressedKeys =
        List.forall (fun k -> List.contains k pressedKeys) testKeys
        
    let matches (pressedKeys: Keystroke.T list) (t: T) =
        let keysCount = List.length pressedKeys
        let lastKey = List.last pressedKeys
        
        if allPressed (modifiers t) pressedKeys then
            if lastKey = (key t)
            then Full
            else Partial
        else if (keysCount > (List.length (keys t)))
            then NoMatch
        else if (List.take keysCount (keys t)) = pressedKeys
            then Partial
        else NoMatch
            
        
//        if keys t |> List.length < keysCount then
//            NoMatch
//        else if (List.take keysCount (keys t)) = pressedKeys then
//            if keysCount = List.length (keys t) then
//                Full
//            else
//                Partial
//        else
//            NoMatch
        
    module Tests =
        open Hotkey
        open FsUnit
        [<TestFixture; Category "Hotkeys">]
        type ``Given a three-key hotkey`` () =
           let hotkey = Windows ^+ Shift ^+ J := (fun r -> Some r)
           
           [<Test>]
           member x.``When I match with the first hotkey I get a partial result`` () =
               hotkey
               |> matches [Keystroke.T.LWin]
               |> should equal Partial
               
           [<Test>]
           member x.``When I match with an unrelated key I get a NoMatch result`` () =
               hotkey
               |> matches [Keystroke.T.F]
               |> should equal NoMatch
               
           [<Test>]
           member x.``When I match with the first two hotkeys I get a partial result`` () =
               hotkey
               |> matches [Keystroke.T.LWin; Keystroke.T.LShiftKey]
               |> should equal Partial
               
           [<Test>]
           member x.``When I match with all the hotkeys I get a Full result`` () =
               hotkey
               |> matches [Keystroke.T.LWin; Keystroke.T.LShiftKey; Keystroke.T.J]
               |> should equal Full

