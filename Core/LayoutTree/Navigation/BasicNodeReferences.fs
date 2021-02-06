namespace Twime
open Twime.LayoutTree
open Twime.TreeNavigation

module BasicNodeReferences =
    let byRoot node = true
    let byExactNode n1 n2 =
        let ref node =
            match node with
            | WindowNode (ref, _) -> ref
            | ContainerNode (ref, _, _) -> ref
        (ref n1) = (ref n2)
    
    let byName name node =
        match node with
          | WindowNode (_, w) -> w.Name = name
          | _ -> false
          
    let byMark mark node =
        match node with
          | WindowNode (_, w) -> List.contains mark w.Definition.marks 
          | _ -> false
          
    let byRef ref node =
        match node with
          | WindowNode (r, _) -> r = ref
          | ContainerNode (r, _, _) -> r = ref
          
    let byHandle handle node =
        match node with
          | WindowNode (_, w) -> w.Definition.handle = handle
          | _ -> false
          
    let byZen node =
        match node with
        | WindowNode (_, w) -> w.Definition.zen
        | _ -> false
        
          
    let byChild matcher node =
        children node
        |> List.exists matcher
    
    let byGrandchild matcher =
        byChild (byChild (matcher))
        
    let byActiveWindow (node: LayoutTree.T) =
        match node with
        | WindowNode (_, w) -> w.Definition.active
        | ContainerNode _ -> false
        
    let byLastActiveWindow (node: LayoutTree.T) =
        match node with
        | WindowNode (_, w) -> w.Definition.lastActiveTrackedWindow
        | ContainerNode _ -> false
        
    let bySelection (node: LayoutTree.T) =
        LayoutTree.isSelected node
        
    let byWindow (window: Window.T) node =
        match node with
        | WindowNode (_, w) -> w.Definition.handle = window.Definition.handle
        | _ -> false
        
    let byPositionIn matcher position tree node =
        withLayout tree {
            let! parent = TreeNavigation.find matcher
            let siblings = TreeNavigation.children parent
            
            let position = List.tryFindIndex (byExactNode node) siblings
                            |> Option.map (fun p -> p = position)
                            
            return position
        } |> Option.defaultValue false
        
    let rootNode node = true
