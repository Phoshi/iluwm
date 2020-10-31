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
            
    let setActiveWindowWeight weight =
        Tree.mapDisplay
            hasActiveWindow
            (Display.mapActiveLayout
                (Weights.setWeight byActiveWindow weight))
    
    let toggleFullScreen =
        let _toggle (node: TreeNavigation.NodeReference) (tree: LayoutTree.T) =
            LayoutTree.withLayout tree {
                return! TreeManipulation.modifyWindow node (fun w -> {w with Definition = {w.Definition with maximised = not w.Definition.maximised}})
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
