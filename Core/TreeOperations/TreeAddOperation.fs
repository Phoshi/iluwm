namespace Twime
open Logume
open TreeManipulation
open BasicNodeReferences
open LayoutTree
open TreeNavigation

module TreeAddOperation =
    let private addAfterWindow (finder: TreeNavigation.NodeReference) (w: Window.Definition.T) t =
        withLayout t { return! addChild (afterNode finder) (byChild finder) (LayoutTree.window w) }
        
    let private _addToDisplayRoot w =
        Display.mapActiveTag
            (Tag.map (addChild toEnd rootNode (LayoutTree.window w)))
            
    let private displayContainsWindow (w: Window.Definition.T) (d: Display.T) =
        Box.contains (Box.midpoint w.size) d.Meta.WorkArea
        
    let addAfterActiveWindowIfSameDisplay log (w: Window.Definition.T) root =
        let activeOnDisplay (w: Window.Definition.T) (d: Display.T) =
            let dcw = displayContainsWindow w d 
            let dcActive = (Display.activeTag d |> Tag.layout |> exists byLastActiveWindow)
            
            Message.message (sprintf "dcw: %b; dcActive: %b" dcw dcActive)
            |> Message.debug
            |> log
            
            dcw && dcActive
                
        if Tree.hasDisplay (activeOnDisplay w) root then
            Message.message "Active display has last active window"
            |> Message.debug
            |> log
            
            Tree.mapLayout
                (exists byLastActiveWindow)
                (addAfterWindow byLastActiveWindow w)
                root
        else None
        
    let addAfterActiveWindow log (w: Window.Definition.T) root =
        if Tree.hasDisplay (Display.activeLayoutHas (exists byLastActiveWindow)) root then 
            Tree.mapLayout
                (exists byLastActiveWindow)
                (addAfterWindow byLastActiveWindow w)
                root
        else None
        
    let addToRootOfClosestDisplay (w: Window.Definition.T) root =
        if Tree.hasDisplay (displayContainsWindow w) root then
            Tree.mapDisplay
                (displayContainsWindow w)
                (_addToDisplayRoot w)
                root
        else None
        
    let addToRootOfPrimaryDisplay (w: Window.Definition.T) =
        Tree.mapDisplay
            (fun d -> d.Meta.Primary)
            (_addToDisplayRoot w)
            
    let ignoreIfAlreadyOnActiveTag (w: Window.Definition.T) tree =
        if Tree.hasDisplay (Display.activeTagHas (Tag.layoutHas (exists (byHandle (w.handle))))) tree then
            Some tree
        else
            None
