namespace Twime

open LayoutTree
open TreeNavigation
open TreeManipulation

module Weights =
    let rec weight (node: T) =
        match node with
        | WindowNode (_, wInfo) -> wInfo.Weight
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
        
    let setWeight (node: NodeReference) (weight: Weight.T) (tree: T) =
        withLayout tree {
            return! modifyWindow node (fun w -> {w with Weight = weight})
        }
        
    let adjustWeight (node: NodeReference) (weight: Weight.T -> Weight.T) (tree: T) =
        withLayout tree {
            return! modifyWindow node (fun w -> {w with Weight = (weight w.Weight)})
        }
