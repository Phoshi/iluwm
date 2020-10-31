namespace Integration
open Integration
open Twime
open Twime.Bar
open Twime.UI
open Twime.LayoutPostProcessors


module Settings =
    type T = {
        hotkeys: HotkeyAction.T list
        initialLayout: Box.T -> string -> bool -> string
        tags: Display.Meta -> Tag.Meta list
        gaps: Gaps option
        windowEventHandlers: WindowEventHandlers option
        uiConfig: UiConfig.T
        uiType: UI.T
        postProcessors: PostProcessors option
        uiSize: int
        workingDirectoryPath: string
    }
    and Gaps = {
        outer: Box.T option
        inner: Box.T option
    }
    and WindowEventHandlers = {
        created: (EventRunner.WindowHandler -> TwimeRoot.T option) option
        destroyed: EventRunner.EventHandler option
        resized: EventRunner.EventHandler option
        titleChanged: EventRunner.EventHandler option
        minimiseChanged: EventRunner.EventHandler option
        maximiseChanged: EventRunner.EventHandler option
        activeChanged: EventRunner.EventHandler option
    }
    and PostProcessors = {
        window: PostProcessor.T option
        ui: PostProcessor.T option
    }

