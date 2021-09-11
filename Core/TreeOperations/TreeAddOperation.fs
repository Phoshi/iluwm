namespace Twime
open System.ComponentModel
open Logume
open Twime.TreeOperation
open TreeManipulation
open BasicNodeReferences
open LayoutTree
open TreeNavigation

module TreeAddOperation =
    let addAfterWindow (finder: NodeReference) (w: Window.Definition.T) t =
        withLayout t {
            let! container = find (byChild finder)
            if container |> containerDefinition |> Option.map Container.exclusive |> Option.defaultValue false then
                return! addChild (afterNode (byChild finder)) (byGrandchild finder) (window w)
            else 
                return! addChild (afterNode finder) (byChild finder) (window w)
        }
        
    let private _addToDisplayRoot w =
        Display.mapActiveTag
            (Tag.map (addChild toEnd rootNode (window w)))
            
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
        
    let addAfter ref (w: Window.Definition.T) root =
        if Tree.hasDisplay (Display.activeLayoutHas (exists ref)) root then 
            Tree.mapLayout
                (exists ref)
                (addAfterWindow ref w)
                root
        else None
        
    let addAfterActiveWindow (w: Window.Definition.T) root =
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
            
    let addAndTabActiveWindow (w: Window.Definition.T) =
        let splitWithNewWindow (activeWindow: T) =
            container
                (Container.create "tabbed" |> Container.withExclusive true)
                [activeWindow; window w]
            
        Tree.mapDisplay
            hasLastActiveWindow
            (Display.mapActiveLayout
                 (replaceNode byLastActiveWindow splitWithNewWindow))
            
    let addAndSplitActiveWindow (ratio: float32) (w: Window.Definition.T) =
        let splitWithNewWindow (activeWindow: T) =
            let weight = Weights.weight activeWindow
            let adjustedWeight = Weight.divide ratio weight
            let adjustedWindow =
                {w with weight = adjustedWeight; ignoreForHigherOrderLayout = true}
            let size =
                windows activeWindow
                |> List.tryHead
                |> Option.map (fun w -> w.Definition.size)
                
            let direction =
                size
                |> Option.map (fun s -> if Box.width s > Box.height s then "horizontal" else "vertical")
                |> Option.defaultValue "horizontal"
                
            container
                (Container.create direction |> Container.withExclusive true)
                [activeWindow; window adjustedWindow]
            
        Tree.mapDisplay
            hasLastActiveWindow
            (Display.mapActiveLayout
                 (replaceNode byLastActiveWindow splitWithNewWindow))

    type SidebarSide = SidebarLeft | SidebarRight
    let addToSidebarOnDisplay side display (w: Window.Definition.T) =
        let (sidebarF, restF, mergeF, engine) =
            match side with
            | SidebarLeft -> (List.tryHead, List.tail, (fun s r -> s @ r), "sidebar-left")
            | SidebarRight -> (List.tryLast, (fun l -> List.take (List.length l - 1) l), (fun s r -> r @ s), "sidebar-right")
            
        let sidebarWindow root =
            let rootInfo =
                containerDefinition root
                |> Option.defaultValue (Container.create "horizontal")
            let children = TreeNavigation.children root
            
            let potentialExistingSidebar =
                children
                |> sidebarF
                |> Option.filter (fun n -> containerDefinition n |> Option.exists (fun c -> c.LayoutEngine = engine))
                
            match potentialExistingSidebar with
            | Some sidebar ->
                let modifiedSidebar =
                    addChild toEnd byRoot (window w) sidebar
                    |> Option.map List.singleton
                    |> Option.defaultValue [sidebar; (window w)]
                container
                    rootInfo
                    (mergeF modifiedSidebar (children |> restF))
            | None ->
                let sidebar =
                    container
                        (Container.create engine |> Container.intransient |> Container.withExclusive true)
                        [window w]
                    
                container
                    rootInfo
                    (mergeF [sidebar] children)
                
        Tree.mapDisplay
            (fun d -> d.Meta.Name = display)
            (Display.mapActiveLayout
                 (replaceNode byRoot sidebarWindow))
        