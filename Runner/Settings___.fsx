        module Config
        open Integration
        open Integration.Hotkey
        open Integration.HotkeyAction
        open Integration.ExecuteOperation
        open Twime.TreeOperation
        open Twime.TreeAddOperation
        open Twime.TreeOperations
        open Twime.TreeRemoveOperation
        open Twime.TreeMoveOperation
        open Twime.TreeUpdateOperation
        open Twime.TreeSplitOperation
        open Twime.TreeTagOperation
        open Twime.TreeChangeWindowSettingOperation
        open Twime.TreeFocusChangeOperation
        open Twime.LayoutPostProcessors
        open Twime
        open Twime.TreeMove
        open Twime.Bar
        open Runner.SettingsReader
        open Runner
        
        type Trashiness =
            | Clean
            | Tight
            | Dirty
            | TrashIncarnate
            
        let trashiness = Clean
        
        let quit _ =
            raise EventRunner.TwimeExit
            None
            
        let hotkeys = ([
            Windows ^+ Hotkey.Up := TreeDirectionalOperation.moveFocus DirectionalNavigation.Direction.Up
            Windows ^+ Hotkey.Left := TreeDirectionalOperation.moveFocus DirectionalNavigation.Direction.Left
            Windows ^+ Hotkey.Down := TreeDirectionalOperation.moveFocus DirectionalNavigation.Direction.Down
            Windows ^+ Hotkey.Right := TreeDirectionalOperation.moveFocus DirectionalNavigation.Direction.Right
            
            Windows ^+ Shift ^+ Hotkey.Up := TreeDirectionalOperation.moveWindow DirectionalNavigation.Direction.Up
            Windows ^+ Shift ^+ Hotkey.Left := TreeDirectionalOperation.moveWindow DirectionalNavigation.Direction.Left
            Windows ^+ Shift ^+ Hotkey.Down := TreeDirectionalOperation.moveWindow DirectionalNavigation.Direction.Down
            Windows ^+ Shift ^+ Hotkey.Right := TreeDirectionalOperation.moveWindow DirectionalNavigation.Direction.Right
            
            Windows ^+ Control ^+ Hotkey.Up := TreeDirectionalOperation.swapWindow DirectionalNavigation.Direction.Up
            Windows ^+ Control ^+ Hotkey.Left := TreeDirectionalOperation.swapWindow DirectionalNavigation.Direction.Left
            Windows ^+ Control ^+ Hotkey.Down := TreeDirectionalOperation.swapWindow DirectionalNavigation.Direction.Down
            Windows ^+ Control ^+ Hotkey.Right := TreeDirectionalOperation.swapWindow DirectionalNavigation.Direction.Right
            
            Windows ^+ Shift ^+ X := changeActiveWindowWeight (Weight.create 0.5 0.0) 
            Windows ^+ X := changeActiveWindowWeight (Weight.create -0.5 -0.0)
            
            Windows ^+ Shift ^+ Z := changeActiveWindowWeight (Weight.create 0.0 -0.5) 
            Windows ^+ Z := changeActiveWindowWeight (Weight.create -0.0 0.5) 
            
            Windows ^+ V := splitActiveWindow "vertical"
            Windows ^+ H := splitActiveWindow "horizontal"
            Windows ^+ S := splitActiveWindowRotate ["horizontal"; "vertical"]
            Windows ^+ R := rotateActiveContainerLayoutEngine ["horizontal"; "vertical"; "tabbed"]
            
            Windows ^+ F := toggleFullScreen
            Windows ^+ M := toggleMinimised
            
            Windows ^+ Q := quit
        ] @
            (seq {
               for key in [D1; D2; D3; D4; D5; D6; D7; D8; D9] do
                   yield Windows ^+ key := setActiveTag (key.ToString().Substring(1))
                   yield Windows ^+ Shift ^+ key := moveActiveWindowToTag (key.ToString().Substring(1))
        } |> Seq.toList))
        
        let initialLayout area name primary =
            if Box.width area > Box.height area
            then "horizontal"
            else "vertical"
        
        let tags display =
            if (Display.primary display) then
                [namedTag "1" "main"; namedTag "4" "secondary"; basicTag "7"]
            else 
                [namedTag "2" "media"; basicTag "5"; basicTag "8"]
        
        let gaps =
            match trashiness with
            | Clean ->
                {
                    inner = gapWidth 30
                    outer = gapWidth 20
                }
            | Tight ->
                {
                    inner = None
                    outer = None
                }
            | Dirty ->
                {
                    inner = gapWidth 40
                    outer = gapWidth 0
                }
            | TrashIncarnate ->
                {
                    inner = Some (Box.create 17 28 -5 7)
                    outer = Some (Box.create 22 21 -18 -29)
                }
        
        let horizontalSignifiers =
            match trashiness with
            | Clean -> ["horizontal"]
            | Tight -> ["horizontal"]
            | Dirty -> ["|"; "horizontal"; "sideways"; "->"; "landscape"]
            | TrashIncarnate -> ["lefty-righty"; "STRIPY"; "wide boi";]
            
        let verticalSignifiers =
            match trashiness with
            | Clean -> ["vertical"]
            | Tight -> ["vertical"]
            | Dirty -> ["-"; "vertical"; "longitudinal"; "^"; "portrait"]
            | TrashIncarnate -> ["uppy-downy"; "STACKED"; "long boi";]
            
        let tabbedSignifiers =
            match trashiness with
            | Clean -> ["tabbed"]
            | Tight -> ["tabbed"]
            | Dirty -> ["🗃"; "tabbed"; "merged"; "X"; "3d"]
            | TrashIncarnate -> ["innie-outie"; "ALL WINDOWS ARE ONE WINDOW"; "deep boi";]
        let uiConfig =
            let trayButton = TrayButton.barComponent (aligned Styling.Right |> andColoured "#d08770" "#2e3440") "🔽" (execute "ActivateTray.exe")
            let windowList = WindowList.barComponent Styling.defaults setFocus
            let tagList = TagList.barComponent Styling.defaults setActiveTag
            let layoutIndicator =
                LayoutIndicator.barComponent
                    (aligned Styling.Right |> andColoured "#bf616a" "#2e3440")
                    (
                        [
                            "horizontal", horizontalSignifiers
                            "vertical", verticalSignifiers
                            "tabbed", tabbedSignifiers
                        ] |> Map.ofList
                    )
                    (rotateLayoutEngine ["horizontal"; "vertical"; "tabbed"])
                    
            ComponentBasedBarConfig.config
                [
                    tagList
                    Spacer.barComponent (coloured "#3b4252" "#123123" |> andPadded 10 10)
                    Clock.barComponent (coloured "#b48ead" "#2e3440" |> andAligned Styling.Right) "fuzzy"
                    Clock.barComponent (coloured "#a3be8c" "#2e3440" |> andAligned Styling.Right) "MMMM d"
                    Clock.barComponent (coloured "#ebcb8b" "#2e3440" |> andAligned Styling.Right) "dddd"
                    trayButton
                    layoutIndicator
                    windowList
                ]
                [
                    layoutIndicator
                    windowList
                ]
                
        let postprocessors =
            match trashiness with
            | TrashIncarnate ->
                Some <|
                    {
                        ui = None
                        window = Some 
                                    (ChainPostProcessor.postprocess [
                                        GapsPostProcessor.postprocess (Box.create 17 28 -5 7)
                                        OffsetPostProcessor.postprocess 100 
                                        ResizePostProcessor.postprocess 20
                                    ])
                    }
            | _ -> None
        
        let uiSize =
            match trashiness with
            | TrashIncarnate -> 75
            | _ -> 25
        
        let settings = {
            hotkeys = hotkeys
            initialLayout = initialLayout
            tags = tags
            gaps = Some gaps
            windowEventHandlers = None
            uiConfig = uiConfig
            uiType = UI.BarUI.ui
            postProcessors = postprocessors
            uiSize = uiSize
        }
