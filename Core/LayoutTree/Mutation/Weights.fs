namespace Twime

open LayoutTree
open TreeNavigation
open TreeManipulation

module Weights =
    let rec weight (node: T) =
        match node with
        | WindowNode (_, wInfo) -> if wInfo.Definition.floating then Weight.zero else wInfo.Definition.weight
        | ContainerNode (_, _, children) -> children
                                            |> List.map weight
                                            |> List.maxBy Weight.total
                                        
    let sumWeights (node: NodeReference) tree =
       withLayout tree {
           let! n = find node
           let immediateChildren = children n
           
           return immediateChildren
                   |> List.fold (fun state child ->
                           let childWeight = weight child
                           Weight.add state childWeight
                       ) Weight.zero 
                   |> Some
       } 
        
    let rec setWeight (node: NodeReference) (weight: Weight.T) (tree: T) =
        if (find node tree |> Option.map LayoutTree.isContainer |> Option.defaultValue false) then
            let windows = 
                withLayout tree {
                    let! container = find node
                    let windows = LayoutTree.windows container
                    return Some windows
                }
            windows
            |> Option.map (List.map (fun w -> setWeight (BasicNodeReferences.byWindow w) weight))
            |> Option.bind (fun trs -> LayoutTree.apply trs tree)
        else
            withLayout tree {
                return! modifyWindow node (fun w -> {w with Definition = {w.Definition with weight = weight}})
            }
        
    let rec adjustWeight (node: NodeReference) (weight: Weight.T -> Weight.T) (tree: T) =
        if (find node tree |> Option.map LayoutTree.isContainer |> Option.defaultValue false) then
            let windows = 
                withLayout tree {
                    let! container = find node
                    let windows = LayoutTree.windows container
                    return Some windows
                }
            windows
            |> Option.map (List.map (fun w -> adjustWeight (BasicNodeReferences.byWindow w) weight))
            |> Option.bind (fun trs -> LayoutTree.apply trs tree)
        else
        withLayout tree {
            return! modifyWindow node (fun w -> {w with Definition = {w.Definition with weight = (weight w.Definition.weight)}})
        }
