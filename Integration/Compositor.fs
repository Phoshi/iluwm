namespace Integration
open Twime.Layout
open Twime
open Twime.LayoutPostProcessors
open Twime.LayoutUIComponents

module Compositor =
    let private _compositTree uiSize ui (box: Box.T) (tree: LayoutTree.T) =
        layout uiSize ui box tree
        
    let postprocess (display: Display.T) postprocessor uiPostprocessor instruction =
        match instruction with
        | Window (w, b) ->
            Window (w, postprocessor display display.Meta.WorkArea (w.Definition.handle.GetHashCode()) (Some w) b)
        | UI uis ->
            UI (
                   uis
                   |> List.map (fun ui ->
                       UIComponent.withBox
                           (uiPostprocessor
                                display
                                display.Meta.WorkArea
                                ((UIComponent.box ui).GetHashCode())
                                None
                                (UIComponent.box ui))
                           ui)
               )
        | _ -> instruction
        
    let private compositDisplay uiSize ui outerGaps post uipost (d: Display.T) =
        let adjustedWorkArea =
            let tag = Display.activeTag d
            let gaps =
                tag.Meta.GapConfig.outer
                |> Option.orElse outerGaps
                |> Option.defaultValue Box.zero
                
            if tag.Meta.GapConfig.enable then
                d.Meta.WorkArea
                |> Box.addBox gaps
            else
                d.Meta.WorkArea
        _compositTree uiSize (ui d) adjustedWorkArea (Display.activeTag d |> Tag.layout)
        |> Option.map (List.map (postprocess d post uipost))
        
    let renderInstructions uiSize ui outerGaps post uipost (root: TwimeRoot.T) =
        (TwimeRoot.displays root)
        |> List.map (compositDisplay uiSize ui outerGaps post uipost)
        |> lift
        |> Option.map (List.collect id)
        
    let invisibleWindows (root: TwimeRoot.T) : Window.T list =
        let rec windowsInLayout (layout: LayoutTree.T) : Window.T list =
            match layout with
            | LayoutTree.WindowNode (_, w) -> [w]
            | LayoutTree.ContainerNode (_, _, children) ->
                List.collect windowsInLayout children
            
        root
        |> TwimeRoot.displays
        |> List.collect (fun d ->
            Display.tags d
            |> List.filter (fun t -> not <| (Display.isActiveTag d t)))
        |> List.map Tag.layout
        |> List.collect windowsInLayout
        
    let activeMonitor root =
        TwimeRoot.displays root
        |> List.tryFind Display.isActive
        
    let rec uiComponents instructions =
        instructions
        |> List.collect (fun i ->
            match i with | UI uis -> uis | _ -> [])
            
    let windowIsZen ((w, _): Window.T * Box.T) =
        w.Definition.zen
            
    let windows (zenWidth, zenHeight) root instructions =
        let displayForBox (b: Box.T) =
            TwimeRoot.displays root
            |> List.find (fun d -> Box.within (Display.workArea d) b)
            
        let zenDisplay (display: Display.T) =
            let workingArea = Display.workArea display
            
            let hSpacing = (Box.width workingArea - zenWidth) / 2
            let vSpacing = (Box.height workingArea - zenHeight) / 2
            
            Box.create
                (Box.left workingArea + hSpacing)
                (Box.top workingArea + vSpacing)
                (Box.right workingArea - hSpacing)
                (Box.bottom workingArea - vSpacing)
            
        let zen (display: Display.T) w = (w, zenDisplay display)
        
        let windows =
            instructions
            |> List.collect (fun i ->
                match i with | Window (w, b) -> [(w, b)] | _ -> [])
            
        if windows |> List.exists windowIsZen then
            windows
            |> List.partition windowIsZen
            |> fun (v, i) ->
                (v |> List.map (fun (w, b) ->
                     zen (displayForBox b) w),
                 i |> List.map (fun (w, _) ->
                     w))
        else
            (windows, [])
        
    let wallpapers root =
        let currentWallpaperOn display =
            let activeTag = Display.activeTag display
            
            let wallpaper = Tag.wallpaper activeTag 
            
            (Display.displayArea display, wallpaper)
            
        TwimeRoot.displays root
        |> List.map currentWallpaperOn
        
    let composit uiSize zenSize (outerGaps: Box.T option) (ui: UI.UI.T) (post: PostProcessor.T) (uipost: PostProcessor.T) _ (root: TwimeRoot.T) =
        maybe {
            let! layoutInstructions = renderInstructions uiSize ui outerGaps post uipost root
            let (visibleWindows, invisibleOnTag) = windows zenSize root layoutInstructions
            let invisibleWindows = (invisibleWindows root) @ invisibleOnTag
            let uis = if List.isEmpty invisibleOnTag then uiComponents layoutInstructions else []
            let wallpapers = wallpapers root
            
            return RenderInstructions.create
                visibleWindows
                invisibleWindows
                (activeMonitor root)
                uis
                root
                wallpapers
        }
        

    module Tests =
        open Twime.LayoutTree.Tests
        open FsUnit
        open NUnit.Framework
        
        let noPost = NoPostProcessor.postprocess
        let noUi _ _ _ _ _ = []
        let composit = composit 0 (0,0) (Some Box.zero) noUi noPost noPost
        [<TestFixture>]
        type ``Given a multi-tag tree`` () =
            let root =
                let tagFor name layout =
                    Tag.create (TreeReference.create ()) (Tag.createMeta name name None None GapConfig.none) layout
                    
                let t1 = C [W "Firefox"] |> mkTree |> tagFor "1"
                let t2 = C [W "Explorer"] |> mkTree |> tagFor "2"
                
                let display =
                    Display.create
                        (TreeReference.create ())
                        (Display.createMeta "" (Box.create 0 0 100 100) (Box.create 0 0 100 100) true false)
                        [t1; t2]
                        t1.Reference 
                        
                TwimeRoot.create [display] []
                
            [<Test>]
            member x.``When I composit the windows, the visible window appears in the visible window block`` () =
                root
                |> composit EventRunner.Maintainance 
                |> Option.get
                |> RenderInstructions.visibleWindows
                |> List.map (fun (wi, _) -> Window.name wi)
                |> should equal [
                    "Firefox"
                ]
                
            [<Test>]
            member x.``When I composit the windows, the invisible window appears in the invisible window block`` () =
                root
                |> composit EventRunner.Maintainance
                |> Option.get
                |> RenderInstructions.invisibleWindows
                |> List.map Window.name
                |> should equal [
                    "Explorer"
                ]
                
        