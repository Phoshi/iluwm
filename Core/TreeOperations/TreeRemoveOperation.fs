namespace Twime
open TreeNavigation
open TreeManipulation
open BasicNodeReferences
open LayoutTree

module TreeRemoveOperation =
    let _removeWindowIfPresent (w: Window.Definition.T) (tree: LayoutTree.T) =
        withLayout tree {
            return! if tree |> exists (byHandle w.handle) then  
                        removeNode (byHandle w.handle) 
                        else Some
        }
        
    let _removeParentIfOnlyWindowLeft (w: Window.Definition.T) (tree: LayoutTree.T) =
        withLayout tree {
            let ascendantRef = byChild (byHandle w.handle)
            
            let! parent = find ascendantRef
            let descendants = children parent
            
            let transient =
                parent
                |> LayoutTree.containerDefinition
                |> Option.map Container.transient
                |> Option.defaultValue true
            
            if List.length descendants = 1 then
                return! removeNode ascendantRef
            else if not transient then
                return! Some
            else if List.length descendants = 2 && (parent <> tree) then
                return! removeNode (byHandle w.handle)
                let! parent = find (byExactNode parent)
                let child = children parent |> List.head
                return! replaceNode (byExactNode parent) (fun _ -> child)
            
            return! Some
        } |> Option.orElse (Some tree)
        
    
    let removeWindow (w: Window.Definition.T): TwimeRoot.Update =
        let _update root =
            maybe {
                let! removeContainersIfRelevant =
                    Tree.mapLayouts
                        (_removeParentIfOnlyWindowLeft w)
                        root
                        
                return! Tree.mapLayouts
                            (_removeWindowIfPresent w)
                            removeContainersIfRelevant
            }
        
        _update
        
    let leaveWindowIfOnlyOnInactiveTag (w: Window.Definition.T) root =
        let windowOnOnlyInactiveTags display =
            let windowOnTag =
                Tag.layoutHas (exists (byHandle w.handle))
                
            let activeTagHasWindow =
                display
                |> Display.activeTag
                |> windowOnTag
            let inactiveTagsHaveWindow =
                display
                |> Display.inactiveTags
                |> List.exists windowOnTag
                
            (not activeTagHasWindow && inactiveTagsHaveWindow)
            
        if Tree.hasDisplay windowOnOnlyInactiveTags root then
            TreeUpdateOperation.updateWindow TreeUpdateOperation.deactivated w root
        else
            None
            
    let leaveWindowIfInZen (w: Window.Definition.T) root =
        let anyWindowIsZen display =
            let windowIsZen =
                Tag.layoutHas (exists byZen)
                
            display
            |> Display.activeTag
            |> windowIsZen
        
        let thisWindowIsZen =
            Tree.hasLayout (fun l ->
                l
                |> find (byHandle w.handle)
                |> Option.bind (fun l -> LayoutTree.windows l |> List.tryHead)
                |> Option.exists (fun w -> w.Definition.zen)
                )
                root
            
        if Tree.hasDisplay anyWindowIsZen root && not thisWindowIsZen then
            TreeUpdateOperation.updateWindow TreeUpdateOperation.deactivated w root
        else
            None
