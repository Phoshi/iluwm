﻿        module Config
        open Integration.Hotkey
        open Integration.HotkeyAction
        open Integration.ExecuteOperation
        open Integration.Settings
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
            Windows ^+ U := TreeDirectionalOperation.moveFocus DirectionalNavigation.Direction.Up
            Windows ^+ N := TreeDirectionalOperation.moveFocus DirectionalNavigation.Direction.Left
            Windows ^+ E := TreeDirectionalOperation.moveFocus DirectionalNavigation.Direction.Down
            Windows ^+ I := TreeDirectionalOperation.moveFocus DirectionalNavigation.Direction.Right
            
            Windows ^+ Shift ^+ U := TreeDirectionalOperation.moveWindow DirectionalNavigation.Direction.Up
            Windows ^+ Shift ^+ N := TreeDirectionalOperation.moveWindow DirectionalNavigation.Direction.Left
            Windows ^+ Shift ^+ E := TreeDirectionalOperation.moveWindow DirectionalNavigation.Direction.Down
            Windows ^+ Shift ^+ I := TreeDirectionalOperation.moveWindow DirectionalNavigation.Direction.Right
            
            Windows ^+ Control ^+ U := TreeDirectionalOperation.swapWindow DirectionalNavigation.Direction.Up
            Windows ^+ Control ^+ N := TreeDirectionalOperation.swapWindow DirectionalNavigation.Direction.Left
            Windows ^+ Control ^+ E := TreeDirectionalOperation.swapWindow DirectionalNavigation.Direction.Down
            Windows ^+ Control ^+ I := TreeDirectionalOperation.swapWindow DirectionalNavigation.Direction.Right
            
            Windows ^+ Shift ^+ Equals := changeActiveWindowWeight (Weight.create 0.5 0.0) 
            Windows ^+ Minus := changeActiveWindowWeight (Weight.create -0.5 -0.0)
            
            
            Windows ^+ BracketOpen := changeActiveWindowWeight (Weight.create 0.0 -0.5) 
            Windows ^+ BracketClose := changeActiveWindowWeight (Weight.create -0.0 0.5) 
            
            Windows ^+ V := splitActiveWindow "vertical"
            Windows ^+ H := splitActiveWindow "horizontal"
            Windows ^+ S := splitActiveWindowRotate ["horizontal"; "vertical"]
            Windows ^+ R := rotateActiveContainerLayoutEngine ["horizontal"; "vertical"; "tabbed"]
            
            Windows ^+ F := toggleFullScreen
            Windows ^+ M := toggleMinimised
            
            Windows ^+ Return := execute "C:\Users\Annabelle\Downloads\cmder\Cmder.exe" ""
            
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
                [fullTag "1" "main" "walls/whitemountains-right.jpg"; fullTag "4" "code" "walls/redmountains-right.jpg"; fullTag "7" "ext" "walls/blackmountains-right.jpg"]
            else 
                [fullTag "2" "IM" "walls/corn-left.jpg"; fullTag "5" "media" "walls/redcat-left.png"; fullTag "8" "watch" "walls/blackbear-left.png"]
        
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
            let discordStatus = DiscordStatus.barComponent (aligned Styling.Left) "%appdata%\BetterDiscord\plugins\iluwm.config.json" true 4
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
                    discordStatus
                    tagList
                    Spacer.barComponent (coloured "#3b4252" "#123123" |> andPadded 10 10)
                    Clock.barComponent (coloured "#b48ead" "#2e3440" |> andAligned Styling.Right) "fuzzy"
                    Clock.barComponent (coloured "#a3be8c" "#2e3440" |> andAligned Styling.Right) "MMMM d"
                    Clock.barComponent (coloured "#ebcb8b" "#2e3440" |> andAligned Styling.Right) "dddd"
                    trayButton
                    layoutIndicator
                    Spacer.barComponent (coloured "#3b4252" "#123123" |> andPadded 10 10 |> andAligned Styling.Right)
                    NowPlaying.barComponent (coloured "#5e81ac" "#d8dee9" |> andAligned Styling.Right)
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
            workingDirectoryPath = "e:\\iluwm\\"
        }
