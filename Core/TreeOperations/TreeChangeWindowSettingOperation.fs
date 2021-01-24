namespace Twime

open BasicNodeReferences
open Twime
open Twime.TreeOperation

module TreeChangeWindowSettingOperation =
    
    let changeActiveWindowWeight delta =
        Tree.mapDisplay
            hasActiveWindow
            (Display.mapActiveLayout
                (Weights.adjustWeight byActiveWindow (Weight.add delta)))
            
    let changeSelectionWeight delta =
        Tree.mapDisplay
            hasActiveWindow
            (Display.mapActiveLayout
                (Weights.adjustWeight bySelection (Weight.add delta)))
            
    let setSelectionWeight weight =
        Tree.mapDisplay
            hasActiveWindow
            (Display.mapActiveLayout
                (Weights.setWeight bySelection weight))
    
    let toggleFullScreen =
        let _toggle (node: TreeNavigation.NodeReference) (tree: LayoutTree.T) =
            LayoutTree.withLayout tree {
                return! TreeManipulation.modifyWindow node (fun w -> {w with Definition = {w.Definition with maximised = not w.Definition.maximised}})
            }
            
        Tree.mapDisplay
            hasActiveWindow
            (Display.mapActiveLayout
                (_toggle byActiveWindow))
            
    let toggleFloating =
        let _toggle (node: TreeNavigation.NodeReference) (tree: LayoutTree.T) =
            LayoutTree.withLayout tree {
                return! TreeManipulation.modifyWindow node (fun w -> {w with Definition = {w.Definition with floating = not w.Definition.floating}})
            }
            
        Tree.mapDisplay
            hasActiveWindow
            (Display.mapActiveLayout
                (_toggle byActiveWindow))
            
    let toggleZen =
        let _toggle (node: TreeNavigation.NodeReference) (tree: LayoutTree.T) =
            LayoutTree.withLayout tree {
                return! TreeManipulation.modifyWindow node (fun w -> {w with Definition = {w.Definition with zen = not w.Definition.zen}})
            }
            
        Tree.mapDisplay
            hasActiveWindow
            (Display.mapActiveLayout
                (_toggle byActiveWindow))
            
    let toggleMinimised =
        let _toggle (node: TreeNavigation.NodeReference) (tree: LayoutTree.T) =
            LayoutTree.withLayout tree {
                return! TreeManipulation.modifyWindow node (fun w -> {w with Definition = {w.Definition with minimised = not w.Definition.minimised}})
            }
            
        Tree.mapDisplay
            hasActiveWindow
            (Display.mapActiveLayout
                (_toggle byActiveWindow))
            
    let setMark ref mark tree =
        let _unsetMark (node: TreeNavigation.NodeReference) (tree: LayoutTree.T) =
            LayoutTree.withLayout tree {
                return! TreeManipulation.modifyWindow node (Window.withDefinition (Window.Definition.removeMark mark))
            }
        let _setMark (node: TreeNavigation.NodeReference) (tree: LayoutTree.T) =
            LayoutTree.withLayout tree {
                return! TreeManipulation.modifyWindow node (Window.withDefinition (Window.Definition.addMark mark))
            }
            
        let unmarkedTree = 
            Tree.mapDisplay
                (hasWindow (byMark mark))
                (Display.mapLayout
                    (fun _ t -> Tag.layoutHas (TreeNavigation.exists (byMark mark)) t)
                    (_unsetMark (byMark mark)))
                tree
            |> Option.defaultValue tree
            
        Tree.mapDisplay
            (hasWindow ref)
            (Display.mapLayout
                (fun _ t -> Tag.layoutHas (TreeNavigation.exists ref) t)
                (_setMark ref))
            unmarkedTree
            
    let setMarkOnActiveWindow mark = setMark byActiveWindow mark
