namespace Twime

open Twime
open Twime.TreeOperation
open TreeNavigation
open TreeManipulation
open BasicNodeReferences
open LayoutTree

module TreeSelectedOperation =
    let reduceSelection =
        let isLastActive parent node =
            parent
            |> LayoutTree.containerDefinition
            |> Option.bind Container.lastActiveChild
            |> Option.contains (LayoutTree.ref node)
        let hoistSelection layout =
            withLayout layout {
                let! selection = find bySelection
                let children = children selection
                let activeChild =
                    children
                    |> List.tryFind (isLastActive selection)
                
                match activeChild with
                | Some activeChild -> 
                    return! replaceNode (byExactNode activeChild) (LayoutTree.selected true)
                    return! replaceNode (byExactNode selection) (LayoutTree.selected false)
                | _ ->
                    return None
            }
            
        Tree.mapLayout
            (exists bySelection)
            hoistSelection

    let expandSelection =
        let hoistSelection layout =
            withLayout layout {
                let! selection = find bySelection
                
                return! replaceNode (byChild bySelection) (LayoutTree.selected true)
                return! replaceNode (byExactNode selection) (LayoutTree.selected false)
            }
            
        Tree.mapLayout
            (exists bySelection)
            hoistSelection
            
    let resetSelection =
        let hoistSelection layout =
            withLayout layout {
                return! replaceNode (bySelection) (LayoutTree.selected false)
                return! replaceNode (byLastActiveWindow) (LayoutTree.selected true)
            }
            
        Tree.mapLayout
            (exists bySelection)
            hoistSelection

