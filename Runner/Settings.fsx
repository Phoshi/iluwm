        module Config
        open Integration.Hotkey
        open Integration.HotkeyAction
        open Integration.ExecuteOperation
        open Integration.Settings
        open Twime.SpecialCase.Action
        open Twime.SpecialCase.Predicate
        open Twime.SpecialCase.Transformer
        open Twime.SpecialCaseAwareOperation
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
        open Twime.TreeChangeTagSettingOperation
        open Twime.TreeSelectedOperation
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
            
            Windows ^+ Shift ^+ Equals := changeSelectionWeight (Weight.create 0.5 0.5) 
            Windows ^+ Minus := changeSelectionWeight (Weight.create -0.5 -0.5)
            
            Windows ^+ Shift ^+ G := toggleGaps
            Windows ^+ Control ^+ Shift ^+ Equals := adjustMaxGap (Box.create 10 10 -10 -10)
            Windows ^+ Control ^+ Minus := adjustMaxGap (Box.create -10 -10 10 10)
            Windows ^+ Control ^+ BracketOpen := adjustOuterGap (Box.create -10 -10 10 10)
            Windows ^+ Control ^+ BracketClose := adjustOuterGap (Box.create 10 10 -10 -10)
            
            Windows ^+ V := splitSelection "vertical"
            Windows ^+ H := splitSelection "horizontal"
            Windows ^+ S := splitSelectionRotate ["horizontal"; "vertical"]
            Windows ^+ R := rotateSelectionLayoutEngine ["horizontal"; "vertical";]
            Windows ^+ T := setSelectionLayoutEngine "tabbed"
            
            Windows ^+ F := toggleFullScreen
            Windows ^+ M := toggleMinimised
            Windows ^+ Z := toggleZen
            Windows ^+ P := toggleFloating
            Windows ^+ Return := execute "C:\Users\Annabelle\Downloads\cmder\Cmder.exe" ""
            
            Windows ^+ Tab := expandSelection
            Windows ^+ Shift ^+ Tab := reduceSelection
            Windows ^+ Delete := resetSelection
            
            Windows ^+ Control ^+ Q := quit

            mode (Windows ^+ K) "mark" 
            mode (Windows ^+ OemTilde) "recall"
            mode F14 "recall"
            
            Windows ^+ Back := focusMark "0"
        ] @
            (seq {
               for key in [A; B; C; D; E; F; G; H; I; J; K; L; M; N; O; P; Q; R; S; T; U; V; W; X; Y; Z;] do
                   yield (key := setMarkOnActiveWindow (key.ToString())) |> inMode "mark"
                   yield (key := focusMark (key.ToString())) |> inMode "recall"
        } |> Seq.toList) @
             (seq {
                for key in [D0; D1; D2; D3; D4; D5; D6; D7; D8; D9] do
                   yield (key := focusMark (key.ToString().Substring(1))) |> inMode "recall"
        } |> Seq.toList) @
             (seq {
                for key in [D1; D2; D3; D4; D5; D6; D7; D8; D9] do
                    yield Windows ^+ key := setActiveTag (key.ToString().Substring(1))
                    yield Windows ^+ Shift ^+ key := moveSelectionToTag (key.ToString().Substring(1))
         } |> Seq.toList))
        
        
        let initialLayout area _ _ =
            if Box.width area > Box.height area
            then "horizontal"
            else "vertical" 
         
        let tags display = 
            if (Display.primary display) then
                [fullTag "1" "α" "walls/whitemountains-right.jpg"; fullTag "4" "β" "walls/redmountains-right.jpg"; fullTag "7" "γ" "walls/whiteflower-right.jpg"; fullTag "9" "δ" "walls/whitetiles-right.png"]
                |> List.map (andWithGaps (GapConfig.create (gapRectangle 90 210) (gapWidth 0) None true))
                
            else 
                [fullTag "2" "IM" "walls/corn-left.jpg"; fullTag "5" "media" "walls/redcat-left.png"; fullTag "8" "watch" "walls/blackbear-left.png"]
                |> List.map (andWithGaps (GapConfig.create (gapRectangle 160 90) (gapWidth 0) (Some (Box.create 0 200 0 0)) true))
        
        let gaps =
            match trashiness with
            | Clean ->
                {
                    inner = gapRectangle 90 100 
                    innerMin = gapWidth 0
                    outer = (Some (Box.create 0 20 0 0))
                }
            | Tight ->
                {
                    inner = None
                    innerMin = None
                    outer = None
                }
            | Dirty ->
                {
                    inner = gapWidth 40
                    innerMin = None
                    outer = gapWidth 0
                }
            | TrashIncarnate ->
                {
                    inner = Some (Box.create 17 28 -5 7)
                    innerMin = None
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
                    Spacer.barComponent (coloured "#3b4252" "#123123" |> andPadded 5 5 |> andAligned Styling.Right)
                    Shell.barComponent (coloured "#ebcb8b" "#2e3440" |> andAligned Styling.Right) @"-file ""e:\Dropbox\the library\the library\.scripts\count-todos.ps1"" -type ""#todo/soon"""
                    Shell.barComponent (coloured "#d08770" "#2e3440" |> andAligned Styling.Right) @"-file ""e:\Dropbox\the library\the library\.scripts\count-todos.ps1"" -type ""#todo/today"""
                    Shell.barComponent (coloured "#bf616a" "#2e3440" |> andAligned Styling.Right) @"-file ""e:\Dropbox\the library\the library\.scripts\count-deferred.ps1"""
                    Spacer.barComponent (coloured "#3b4252" "#123123" |> andPadded 5 5 |> andAligned Styling.Right)
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
                                        GapsPostProcessor.postprocess PostProcessor.ProcessingStyle.Window (Box.create 17 28 -5 7)
                                        OffsetPostProcessor.postprocess 100 
                                        ResizePostProcessor.postprocess 20
                                    ])
                    }
            | _ -> None
        
        let uiSize =
            match trashiness with
            | TrashIncarnate -> 75
            | _ -> 25
            
        let specialCases = [
            executable "discord" => transform (properties [weight (0.5, 0.5); mark "D"]) (addToSidebarOnDisplay SidebarLeft "\\\\.\DISPLAY1")
            executable "slack" => transform (properties [weight (0.5, 0.5); mark "S"]) (addToSidebarOnDisplay SidebarLeft "\\\\.\DISPLAY1")
            executable "Telegram" => transform (mark "T") addAfterActiveWindow
            executable "KeePass" => addAndSplitActiveWindow (60.0f/40.0f)
            executable "KeePassXC" => addAndSplitActiveWindow (60.0f/40.0f)
            executable "ConEmu64" => addAndSplitActiveWindow (70.0f/30.0f)
            executable "foobar2000" => addToTag "5"
            executable "steam" => transform (weight (0.5, 0.5)) addAfterActiveWindow
            executable "obsidian" => transform (properties [weight (0.5, 0.5); mark "O"]) (addToSidebarOnDisplay SidebarRight "\\\\.\DISPLAY1")
        ]
        
        let settings = {
            hotkeys = hotkeys
            initialLayout = initialLayout
            tags = tags
            gaps = Some gaps
            specialCases = specialCases
            windowEventHandlers = None
            uiConfig = uiConfig
            uiType = UI.BarUI.ui
            postProcessors = postprocessors
            uiSize = uiSize
            workingDirectoryPath = "e:\\iluwm\\"
            logLevel = Logume.Errors
        }
