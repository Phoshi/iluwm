namespace Twime

open Twime.TreeMove
open Twime.LayoutTree
open Twime.TreeNavigation
open Twime.BasicNodeReferences
open Twime.TreeOperation

module TreeFocusChangeOperation =
    let setFocus (ref: TreeReference.T) root =
        let hasRef = BasicNodeReferences.byRef ref
        switchFocus byActiveWindow hasRef root
        
    let moveFocus (direction: Direction) root =
        let activeLayout =
            TwimeRoot.display
                hasActiveWindow
                root
            |> Display.activeTag
            |> Tag.layout
            
        let windows =
            activeLayout
            |> LayoutTree.windows
            
        let indexOfCurrentWindow =
           windows
           |> List.findIndex
               (fun w -> Window.Definition.active (Window.definition w))
               
        let newIndex = indexOfCurrentWindow + (match direction with | Up -> -1 | Down -> 1)
        
        if newIndex < 0 || newIndex >= List.length windows then
            None
        else
            let currentWindow = List.item indexOfCurrentWindow windows
            let newWindow = List.item newIndex windows
            
            switchFocus (byWindow currentWindow) (byWindow newWindow) root
            

    let updateActiveDisplay name root =
        maybe {
            let! disabledTree = Tree.mapDisplays
                                    (Display.setActive false)
                                    root
            return! Tree.mapDisplay (Display.byName name) (Display.setActive true) disabledTree
        }
        
    let moveFocusBetweenMonitors (direction: Direction) root =
        let currentDisplay =
            root
            |> TwimeRoot.display
                    Display.isActive
                   
        let next =
            TreeMoveOperation.nextDisplay direction currentDisplay root
            
        maybe {
            let! windowToActivate =
                TwimeRoot.display
                    (Display.activeTagHas
                        (Tag.layoutHas (TreeNavigation.exists next)))
                    root
                |> Display.activeTag
                |> Tag.layout
                |> LayoutTree.windows
                |> List.tryHead
            let! deactivatedTree = Tree.mapDisplay
                                        (hasWindow byActiveWindow)
                                        (Display.mapActiveLayout
                                            (TreeManipulation.modifyWindow byActiveWindow (Window.withDefinition (Window.Definition.withActive false))))
                                        root
            let! reactivatedTree = Tree.mapDisplay
                                        (hasWindow (byWindow windowToActivate))
                                        (Display.mapActiveLayout
                                            (TreeManipulation.modifyWindow (byWindow windowToActivate) (Window.withDefinition (Window.Definition.withActive true))))
                                        deactivatedTree
                                        
            return reactivatedTree
        }
        |> Option.orElse (updateActiveDisplay ((TwimeRoot.display (Display.activeTagHas (Tag.layoutHas (TreeNavigation.exists next))) root) |> Display.info |> Display.name) root)
    
    
