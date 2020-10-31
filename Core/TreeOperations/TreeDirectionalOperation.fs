namespace Twime.TreeOperations

open Twime
open Twime.DirectionalNavigation
open BasicNodeReferences
open TreeNavigation
open LayoutTree

module TreeDirectionalOperation =
    let layoutFor (monitor: Display.T) =
        Layout.layout 0 (fun _ _ _ _ -> []) monitor.Meta.WorkArea
        
    let applyResult fWindow fBoundary result (root: TwimeRoot.T) =
        match result with
        | Some dr ->
            match dr with
            | DirectionalResult.Boundary -> fBoundary root
            | DirectionalResult.Window w -> fWindow w root
        | _ -> None
        
    let directionIsDeeper direction =
        direction = Direction.Down || direction = Direction.Right
    let directionMatchesContainer direction container =
        let engine = LayoutTree.layoutEngine container
        
        if engine = "vertical" then
            match direction with
            | Direction.Down -> true
            | Direction.Up -> true
            | _ -> false
        else
            match direction with
            | Direction.Left -> true
            | Direction.Right -> true
            | _ -> false
            
    let insertionPoint direction =
        if direction = Direction.Left || direction = Direction.Up
        then TreeManipulation.toStart
        else TreeManipulation.toEnd
        
    let fail _ = None
    
    let newContainerIn direction node root =
        let wrapLayoutInContainer direction tree =
            LayoutTree.T.ContainerNode
                (TreeReference.create (),
                 Container.create direction,
                 [tree])
            |> Some
        let engine =
            if direction = Direction.Down || direction = Direction.Up
            then "vertical"
            else "horizontal"
                
        withLayout root {
            return! wrapLayoutInContainer engine 
            return! TreeMove._move byRoot (insertionPoint direction) byActiveWindow
        }
    
    let rec firstParentWhere pred node root =
        match parent node root with
        | Some directParent ->
            if pred directParent then
                Some directParent
            else
                firstParentWhere pred directParent root
        | _ -> None
    
    let containerOfType engine container =
        LayoutTree.layoutEngine container = engine
        
    let typeForDirection d =
        match d with
        | Right -> "horizontal"
        | Left -> "horizontal"
        | Up -> "vertical"
        | Down -> "vertical"
        
    let iterativeBreakout direction = 
        TreeOperation.onActiveLayout (fun root ->
            withLayout root {
                let! active = find byActiveWindow
                let! container = parent active
                let superContainer = firstParentWhere (containerOfType (typeForDirection direction)) container root
                
                match superContainer with
                | Some target ->
                    return! TreeMove._move
                                (byExactNode target)
                                (insertionPoint direction)
                                byActiveWindow
                | _ ->
                    return! newContainerIn direction byActiveWindow 
            }
        )
    
    let _switchFocus w root =
        TreeOperation.switchFocus (BasicNodeReferences.byActiveWindow) (BasicNodeReferences.byWindow w) root
        
    let _swapWindow w =
        TreeOperation.onActiveLayout (fun root ->
            withLayout root {
                let oneRef = byWindow w
                let! one = find oneRef
                let! two = find byActiveWindow
                
                return! TreeMove._swap one two
            }
        )
    let _moveWindow direction w =
        let sameContainer oneRef twoRef root =
            withLayout root {
                let! one = find oneRef
                let! two = find twoRef
                
                let! c1 = parent one
                let! c2 = parent two
                
                return Some (c1 = c2)
            }
            
        let targetPosition sameContainer directionMatchesContainer direction =
            if sameContainer && directionIsDeeper direction then
                TreeManipulation.afterNode
            else if not sameContainer && not (directionIsDeeper direction) && directionMatchesContainer then 
                TreeManipulation.afterNode
            else 
                TreeManipulation.beforeNode
                
        TreeOperation.onActiveLayout (fun root -> 
            withLayout root {
                let targetReference = byWindow w
                let! sameContainer = sameContainer targetReference byActiveWindow
                let! target = find targetReference
                let! targetContainer = parent target
                
                let targetPosition =
                    (targetPosition
                         sameContainer
                         (directionMatchesContainer direction targetContainer)
                         direction)
                         targetReference
                
                return! TreeMove._move (byChild targetReference) targetPosition byActiveWindow
            }
        )
        
    let moveFocus direction root =
        let currentMonitor =
            TreeOperation.activeDisplay
                root
                
        let newActive = DirectionalNavigation.windowTo (layoutFor currentMonitor) direction BasicNodeReferences.byActiveWindow (Display.activeLayout currentMonitor)
        
        applyResult _switchFocus fail newActive root
        
    let swapWindow direction root =
        let currentMonitor =
            TreeOperation.activeDisplay
                root
                
        let newActive = DirectionalNavigation.windowTo (layoutFor currentMonitor) direction BasicNodeReferences.byActiveWindow (Display.activeLayout currentMonitor)
        
        applyResult _swapWindow fail newActive root
        
    let moveWindow direction root =
        let currentMonitor =
            TreeOperation.activeDisplay
                root
                
        let newActive = DirectionalNavigation.windowTo (layoutFor currentMonitor) direction BasicNodeReferences.byActiveWindow (Display.activeLayout currentMonitor)
        
        applyResult (_moveWindow direction) (iterativeBreakout direction) newActive root
