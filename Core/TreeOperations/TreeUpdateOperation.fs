namespace Twime
open System.ComponentModel
open System.ComponentModel
open TreeManipulation
open TreeNavigation
open BasicNodeReferences
open LayoutTree
open Twime.TreeMove

module TreeUpdateOperation =
    let _updateWindowIfPresent (w: Window.Definition.T) f (tree: LayoutTree.T) =
        withLayout tree {
            return! if tree |> exists (byHandle w.handle) then  
                        modifyWindow (byHandle w.handle) f
                        else Some
        }
    let _updateWindow (w: Window.Definition.T) (f: Window.Definition.T -> Window.Definition.T): TwimeRoot.Update =
        let _update root =
            Tree.mapLayouts
                (_updateWindowIfPresent
                     w
                     (fun w -> Window.create (w.Name) (f w.Definition))
                )
                root
            
        _update
        
    let size (newSize: Window.Definition.T) (w: Window.Definition.T): Window.Definition.T =
        {w with size = newSize.size}
        
    let title (newTitle: Window.Definition.T) (w: Window.Definition.T): Window.Definition.T =
        {w with title = newTitle.title}
        
    let minimised (n: Window.Definition.T) (w: Window.Definition.T) =
        {w with minimised = n.minimised}
        
    let maximised (n: Window.Definition.T) (w: Window.Definition.T) =
        {w with maximised = n.maximised}
        
    let activated (n: Window.Definition.T) (w: Window.Definition.T) =
        {w with active = n.active}
        
    let setActive a (n: Window.Definition.T) (w: Window.Definition.T) =
        {w with active = a}
        
    let deactivated (n: Window.Definition.T) (w: Window.Definition.T) =
        {w with active = false}
        
    let lastActive (n: Window.Definition.T) (w: Window.Definition.T) =
        {w with lastActiveTrackedWindow = true}
        
    let notLastActive (n: Window.Definition.T) (w: Window.Definition.T) =
        {w with lastActiveTrackedWindow = false}
        
    let selected (n: Window.Definition.T) (w: Window.Definition.T) =
        {w with selected = true}
        
    let notSelected (n: Window.Definition.T) (w: Window.Definition.T) =
        {w with selected = false}
        
    let updateWindow (f) (w: Window.Definition.T): TwimeRoot.Update =
        _updateWindow w (f w)
        
    let updateLastActiveTree (active: NodeReference) (tree: TwimeRoot.T) =
        let rec _update active l =
            withLayout l {
               let! node = find active
               let! parent = parent node
               
               return! TreeManipulation.modifyContainer (byExactNode parent) (Container.withLastActive (Some <| LayoutTree.ref node))
               
               if exists (byChild active) l then
                   return! _update (byExactNode parent)
                   
            }
            |> Option.orElse (Some l)
            
        Tree.mapDisplay
            (Display.activeLayoutHas (exists active))
            (Display.mapActiveLayout (_update active))
            tree
            
    let updateLastWindowMark (w: Window.Definition.T) (tree: TwimeRoot.T) =
        let shiftMark toRef oldMark newMark =
            Tree.apply
                [
                    Tree.mapWindow (byMark oldMark) (Window.withDefinition (Window.Definition.addMark newMark))
                    Tree.mapWindow (byMark oldMark) (Window.withDefinition (Window.Definition.removeMarks (fun m -> m <> newMark && m.ToCharArray() |> Array.forall System.Char.IsDigit))) 
                ]
        if w.active then
            Tree.apply
                [
                    Tree.mapWindow (byMark "9") (Window.withDefinition (Window.Definition.removeMark "9")) |> Tree.relaxed
                    shiftMark (byMark "8") "8" "9" |> Tree.relaxed
                    shiftMark (byMark "7") "7" "8" |> Tree.relaxed
                    shiftMark (byMark "6") "6" "7" |> Tree.relaxed
                    shiftMark (byMark "5") "5" "6" |> Tree.relaxed
                    shiftMark (byMark "4") "4" "5" |> Tree.relaxed
                    shiftMark (byMark "3") "3" "4" |> Tree.relaxed
                    shiftMark (byMark "2") "2" "3" |> Tree.relaxed
                    shiftMark (byMark "1") "1" "2" |> Tree.relaxed
                    shiftMark (byMark "0") "0" "1" |> Tree.relaxed
                    Tree.mapWindow (byLastActiveWindow) (Window.withDefinition (Window.Definition.removeMarks (fun m -> m.ToCharArray() |> Array.forall System.Char.IsDigit))) 
                    Tree.mapWindow (byLastActiveWindow) (Window.withDefinition (Window.Definition.addMark "0"))
                ]
                tree
                
        else Some tree
        
    let unmarkActiveWindow (w: Window.Definition.T) (tree: TwimeRoot.T) =
        if w.active then
                Tree.mapWindow
                    (byLastActiveWindow)
                    (Window.withDefinition (Window.Definition.removeMarks (fun m -> m.ToCharArray() |> Array.forall System.Char.IsDigit)))
                    tree
        else Some tree
    
    let updateWindowActive (w: Window.Definition.T) (tree: TwimeRoot.T) =
        let allWindows =
            TwimeRoot.displays tree
            |> List.collect Display.tags
            |> List.map Tag.layout
            |> List.collect LayoutTree.windows
            |> List.map (fun w -> w.Definition)
        
        if w.active then
            let mutable t = Some tree
            
            for window in allWindows do
                t <- t |> Option.map (updateWindow (setActive false) window) |> Option.flatten
            
            t <- t |> Option.map (updateWindow (setActive true) w) |> Option.flatten
                
            t
                
        else Some tree
        
    let updateWindowLastActive (w: Window.Definition.T) (tree: TwimeRoot.T) =
        let allWindows =
            TwimeRoot.displays tree
            |> List.collect Display.tags
            |> List.map Tag.layout
            |> List.collect LayoutTree.windows
            |> List.map (fun w -> w.Definition)
        
        if w.active then
            let mutable t = Some tree
            
            for window in allWindows do
                t <- t |> Option.map (updateWindow notLastActive window) |> Option.flatten
            
            t <- t |> Option.map (updateWindow lastActive w) |> Option.flatten
            
            t <- t |> Option.map (updateLastActiveTree byLastActiveWindow) |> Option.flatten
                
            t
                
        else Some tree
        
    let updateSelected (w: Window.Definition.T) (tree: TwimeRoot.T) =
        if w.active then
            let mutable t = Some tree
            
            let fWin (r, w) =
                WindowNode (r, w |> Window.withDefinition (Window.Definition.withSelected false))
            let fContainer (r, c, ch) =
                ContainerNode (r, c |> Container.withSelected false, ch)
            t <-
                t
                |> Option.map
                       (Tree.mapLayouts
                            (fun l -> Some
                                        (cataTree
                                            fWin
                                            fContainer
                                            l)))
                |> Option.flatten
            
            t <- t |> Option.map (updateWindow selected w) |> Option.flatten
                
            t
                
        else Some tree
    
    
