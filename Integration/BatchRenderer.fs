namespace Integration

open System
open System.Windows
open System.Windows
open System.Windows
open Integration.Win32
open Integration.Win32
open Logume
open Twime
open Twime.Bar
open Twime.LayoutUIComponents
open Views
open Views.Bar

module BatchRenderer =
    module Settings =
        type T = {
            updateMousePosition: bool
            windowMovementStyle: MovementStyle
        }
        and MovementStyle = Batch | OneByOne
        
        let create updateMousePosition movementStyle =
            {updateMousePosition = updateMousePosition; windowMovementStyle = movementStyle}
            
    let overrideUserFocus t =
        match t with
        | EventRunner.UserDriven -> true
        | EventRunner.UiDriven -> true
        | _ -> false
    let bePolite (windows: RenderInstructions.T) updateType =
        if overrideUserFocus updateType then
            false 
        else
            let foreground = WindowState.foregroundWindow ()
            
            windows
            |> RenderInstructions.visibleWindows
            |> List.exists (fun (w, _) -> (WindowHandle.fromWindow w.Definition) = foreground)
            |> not
        
        
    let placeMouseInWindow windowPosition =
        let cursorPosition = CursorMove.getCursorPosition ()
        
        if not (Box.contains cursorPosition windowPosition) then
            let (x, y) = Box.midpoint windowPosition
            CursorMove.setCursorPosition x y
        
        
    let overrideCursorPosition t =
        match t with
        | EventRunner.UserDriven -> true
        | _ -> false
        
    let moveWindows t =
        match t with
        | EventRunner.UserDriven -> true
        | EventRunner.UiDriven -> true
        | EventRunner.HighPriority -> true
        | EventRunner.LowPriority -> true
        | _ -> false
        
    type TempDC = {
        Children: Child list
    }
    and Child = {
        Name: string
        Active: bool 
    }
    
    let exec (f: unit -> unit) =
         Application.Current.Dispatcher.Invoke(f)
    
    let mutable allUis: (LayoutTree.T * Window) list = []
    let registerUi container ui =
        allUis <- (container, ui) :: allUis
        
    let uiForParent container =
        allUis
        |> List.tryFind (fun (p, _) -> p = container)
        |> Option.map (fun (_, w) -> w)
            
    let removeUi (conf: UiConfig.T) (ui: Window) =
        fun () -> UiConfig.destroy conf ui
        |> exec
        
        allUis <- List.filter (fun (_, u) -> u <> ui) allUis
        
    let newWindow submit conf uic root =
        let b = UiConfig.create conf root submit uic 
        
        registerUi (UIComponent.container uic) b
        
    let createUi submit conf ui root =
        fun () ->
            newWindow submit conf ui root
        |> exec
            
    let updateUi submit conf window ui root =
        fun () -> UiConfig.update conf window root submit ui
        |> exec
        
    let destroyedUis uis =
        let allCurrentParents =
            uis
            |> List.map UIComponent.container
            
        allUis
        |> List.filter (fun (p, _) -> not <| List.contains p allCurrentParents)
        |> List.map (fun (_, w) -> w)
        
    let newUis uis =
        let newUi ui =
           uiForParent (UIComponent.container ui)
           |> Option.isNone
            
        uis
        |> List.filter newUi
        
    let existingUis uis =
        seq {
            for ui in uis do
                let existingUi = uiForParent (UIComponent.container ui)
                if Option.isSome existingUi then
                    yield (Option.get existingUi, ui)
        }
        
    let renderUis submit conf uis root =
        for ui in newUis uis do
            createUi submit conf ui root
        for (window, ui) in existingUis uis do
            updateUi submit conf window ui root
        for ui in destroyedUis uis do
            removeUi conf ui
            
    let changed windows =
        windows
        |> RenderInstructions.visibleWindows
        |> List.filter (fun (w, p) -> w.Definition.size <> p)
        |> List.filter (fun (w, _) -> not (WindowState.isMaximised (WindowHandle.fromWindow w.Definition)))
        |> List.map (fun (w, p) -> (w.Definition, p))
        
    let visible windows =
        windows
        |> RenderInstructions.visibleWindows
        |> List.filter (fun (w, _) -> not (WindowState.isVisible (WindowHandle.fromWindow w.Definition)))
        |> List.map (fun (w, p) -> (w.Definition, p))
        
    let invisible windows =
        windows
        |> RenderInstructions.invisibleWindows
        |> List.filter (fun (w) ->  (WindowState.isVisible (WindowHandle.fromWindow w.Definition)))
        |> List.map (fun w -> w.Definition)
        
    let individualUpdateWindows log windows =
        Message.message "Rendering changes"
        |> Message.addGauge "Num" (List.length (changed windows) |> string)
        |> Message.debug
        |> log
        
        WindowMove.batchVisibilityChange
            (visible windows)
            (invisible windows)
            
        WindowMove.moveWindows
            (changed windows)
        
            
    let batchUpdateWindows log (windows: RenderInstructions.T) =
        Message.message "Rendering changes"
        |> Message.addGauge "Num" (List.length (changed windows) |> string)
        |> Message.debug
        |> log
            
        WindowMove.batchVisibilityChange
            (visible windows)
            (invisible windows)
            
        WindowMove.batchMove
            (changed windows)
        
            
    let render log conf (settings: Settings.T) (submitCommand: EventRunner.Run) (updateType: EventRunner.UpdateType) (windows: RenderInstructions.T) =
        renderUis submitCommand conf (RenderInstructions.uis windows) (RenderInstructions.root windows)
        Wallpaper.set (RenderInstructions.wallpaper windows)
        
        if moveWindows updateType then
            match settings.windowMovementStyle with
            | Settings.Batch -> batchUpdateWindows log windows
            | Settings.OneByOne -> individualUpdateWindows log windows
            
            let mismaximisedWindows =
                windows
                |> RenderInstructions.visibleWindows
                |> List.filter (fun (w, _) -> w.Definition.maximised <> WindowState.isMaximised (WindowHandle.fromWindow w.Definition))
                
            for (w, p) in mismaximisedWindows do
                let handle =
                    (WindowHandle.fromWindow w.Definition)
                if w.Definition.maximised then
                    WindowState.maximise handle
                else
                    WindowState.restore handle
                    
            let misminimisedWindows =
                windows
                |> RenderInstructions.visibleWindows
                |> List.filter (fun (w, _) -> w.Definition.minimised <> WindowState.isMinimised (WindowHandle.fromWindow w.Definition))
                
            for (w, p) in misminimisedWindows do
                let handle =
                    (WindowHandle.fromWindow w.Definition)
                if w.Definition.minimised then
                    WindowState.minimise handle
                else
                    WindowState.restore handle
            
        if not (bePolite windows updateType) then
            let activeWindow =
                windows
                |> RenderInstructions.visibleWindows
                |> List.tryFind (fun (w, _) -> w.Definition.active)
                
            Message.message "Active window is tracked"
            |> Message.debug
            |> log
            
            for (active, position) in activeWindow |> Option.toList do 
                if (not (WindowState.isForeground (WindowHandle.fromWindow active.Definition))) && (not active.Definition.minimised) then
                    WindowState.setForeground (WindowHandle.fromWindow active.Definition)
                    Message.message "Asserting foreground status"
                    |> Message.debug
                    |> log
                    
                if settings.updateMousePosition && overrideCursorPosition updateType then
                    placeMouseInWindow position
                    Message.message "Asserting mouse position in window"
                    |> Message.debug
                    |> log
                
            if Option.isNone activeWindow then
                for display in RenderInstructions.activeDisplay windows |> Option.toList do
                    placeMouseInWindow (Display.workArea display)
                    Message.message "Asserting mouse position in monitor"
                    |> Message.debug
                    |> log
