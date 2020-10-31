namespace Twime
open Twime.LayoutTree
open Twime.TreeNavigation
open Twime.BasicNodeReferences
open Twime.TreeManipulation
open NUnit.Framework

module TreeMove =
    type private BoundingStatus =
    | BeforeContainer
    | InsideContainer
    | AfterContainer
    
    type private NodePosition =
    | Start
    | Middle of int
    | End
    
    let private positionWithinContainer container node =
        let nodes = children container
        let index = List.findIndex node nodes
        let count = List.length nodes
        
        match index with
            | 0 -> Start
            | _ when index = count - 1 -> End
            | _ -> Middle index
            
        
    let private isIndexInsideContainer container index =
        if index < 0 then BeforeContainer
        else if index >= List.length (children container) then AfterContainer
        else InsideContainer
        
        
    type Direction = Up | Down
    type BreakoutState = NotBreaking | Breaking
    let private moveTarget (direction: Direction) (node: NodeReference) (tree: T) =
        let rec _moveTarget breakoutState direction node tree =
            withLayout tree {
                let! container = find (byChild node)
                let siblings = children container
                
                let position = positionWithinContainer container node
                
                let startIndex =
                    match position with
                    | Start -> 0
                    | Middle i -> i
                    | End -> List.length siblings - 1
                    
                let newIndex =
                    match direction with
                    | Up -> startIndex - 1
                    | Down -> startIndex + 1
                    
                return match isIndexInsideContainer container newIndex with
                        | BeforeContainer ->
                            match breakoutState with
                            | NotBreaking -> _moveTarget Breaking direction (byExactNode container) tree
                            | Breaking -> Some (toStart, byExactNode container)
                        | InsideContainer ->
                            match breakoutState with
                            | NotBreaking ->
                                let nodeInNewPosition = List.item newIndex siblings
                                match nodeInNewPosition with
                                | WindowNode _ -> Some (atIndex newIndex, (byExactNode container))
                                | ContainerNode _ -> Some ((match direction with | Up -> toEnd | Down -> toStart), byExactNode nodeInNewPosition)
                            | Breaking -> Some (atIndex (newIndex + (match direction with | Up -> 1 | Down -> 0)), (byExactNode container))
                        | AfterContainer ->
                            match breakoutState with
                            | NotBreaking -> _moveTarget Breaking direction (byExactNode container) tree
                            | Breaking -> Some (toEnd, byExactNode container)
            }
        _moveTarget NotBreaking direction node tree
        
    let _move container position nodeReference tree =
        withLayout tree {
            let! movingNode = find nodeReference
            let! movingParent = parent movingNode
            
            return! removeNode nodeReference
            return! addChild position container movingNode
            return! deleteEmptyContainers
            return! deleteContainerIfSingleChild (byExactNode movingParent)
        }
    let move (direction: Direction) (node: NodeReference) (tree: T) =
        withLayout tree {
            let! (position, container) = moveTarget direction node
            
            return! _move container position node
        }
        
    let _swap (first: LayoutTree.T) (second: LayoutTree.T) tree =
        withLayout tree {
            let firstRef = byExactNode first
            let secondRef = byExactNode second
            
            let firstWeight = Weights.weight first
            let secondWeight = Weights.weight second
            
            return! Weights.setWeight firstRef secondWeight
            return! Weights.setWeight secondRef firstWeight
            
            let! firstNode = find firstRef
            let! secondNode = find secondRef
            
            return! replaceNode firstRef (fun _ -> LayoutTree.clone secondNode)
            return! replaceNode secondRef (fun _ -> LayoutTree.clone firstNode)
            
        }
        
    let swap (direction: Direction) (node: NodeReference) (tree: T) =
        let swappingWindows = maybe {
            let windows = LayoutTree.windowNodes tree
            let! swappingNode = List.tryFind node windows
            let! swappingIndex = List.tryFindIndex ((=) swappingNode) windows
            
            let offset = match direction with Up -> -1 | Down -> 1
            
            let! swappedNode = List.tryItem (swappingIndex + offset) windows
            
            return (swappingNode, swappedNode)
        }
        
        match swappingWindows with
        | Some (first, second) -> _swap first second tree
        | _ -> None
        
    module Tests =
        open Twime.LayoutTree.Tests
        open BeSameTreeAs
        open FsUnit
        
        [<TestFixture; Category "move">]
        type ``Given a flat tree with three items`` () =
            let tree = mkTree <| C [W "Rider"; W "Firefox"; W "Explorer"]
            
            [<Test>]
            member x.``when I move the bottom item up twice it is at the top``() =
                withLayout tree {
                    return! move Up <| byName "Explorer"
                    return! move Up <| byName "Explorer"
                }
                |> should beSameTreeAs (mkTree <| C [W "Explorer"; W "Rider"; W "Firefox"])
                
            [<Test>]
            member x.``when I move the bottom item up and then down again then the tree is unchanged``() =
                withLayout tree {
                    return! move Up <| byName "Explorer"
                    return! move Down <| byName "Explorer"
                }
                |> should equal <| Some tree
        
        [<TestFixture; Category "move">]
        type ``Given a tree with nested containers`` () =
            let tree = mkTree <| C [W "Rider"; C [W "Firefox"; W "Explorer"]]
            
            [<Test>]
            member x.``when I move a node down it is reordered``() =
                move Down (byName "Firefox") tree
                |> should beSameTreeAs (mkTree (C [W "Rider"; C [W "Explorer"; W "Firefox"]])) 
                
            [<Test>]
            member x.``when I move a node up it is reordered``() =
                move Up (byName "Explorer") tree
                |> should beSameTreeAs (mkTree (C [W "Rider"; C [W "Explorer"; W "Firefox"]]))
                
        [<TestFixture; Category "move">]
        type ``Given a tree with containers as siblings``() =
            let tree = mkTree <| C [W "Rider"; C [W "Discord"; W "Telegram"]; C [W "Firefox"; W "Explorer"]]
                
            [<Test>]
            member x.``when I move a node at the bottom edge of its container down it breaks out``() =
               move Down (byName "Explorer") tree
               |> should beSameTreeAs (mkTree (C [W "Rider"; C [W "Discord"; W "Telegram"]; W "Firefox"; W "Explorer"]))
               
            [<Test>]
            member x.``when I move a node at the top edge of its container down it moves down``() =
               move Down (byName "Firefox") tree
               |> should beSameTreeAs (mkTree (C [W "Rider"; C [W "Discord"; W "Telegram"]; C [W "Explorer"; W "Firefox"]]))
               
            [<Test>]
            member x.``when I move a node at the edge of its container down it breaks out, even when the next sibling is also a container``() =
                move Down (byName "Telegram") tree
                |> should beSameTreeAs (mkTree <| C [W "Rider"; W "Discord"; W "Telegram"; C [W "Firefox"; W "Explorer"]])
                
            [<Test>]
            member x.``when I move a node down into a container, it enters at the start``() =
                withLayout tree {
                    return! move Down (byName "Telegram") 
                    return! move Down (byName "Telegram")
                }
                |> should beSameTreeAs (mkTree <| C [W "Rider"; W "Discord"; C [W "Telegram"; W "Firefox"; W "Explorer"]])
                
                
            [<Test>]
            member x.``when I move a node at the top edge of its container up it breaks out``() =
               move Up (byName "Firefox") tree
               |> should beSameTreeAs (mkTree (C [W "Rider"; C [W "Discord"; W "Telegram"]; W "Firefox"; W "Explorer"]))
               
            [<Test>]
            member x.``when I move a node at the bottom edge of its container up it moves up``() =
               move Up (byName "Explorer") tree
               |> should beSameTreeAs (mkTree (C [W "Rider"; C [W "Discord"; W "Telegram"]; C [W "Explorer"; W "Firefox"]]))
               
            [<Test>]
            member x.``when I move a node at the top edge of its container up it breaks out, even when the next sibling is also a container``() =
                move Up (byName "Firefox") tree
                |> should beSameTreeAs (mkTree <| C [W "Rider"; C [W "Discord"; W "Telegram"]; W "Firefox"; W "Explorer"])
                
            [<Test>]
            member x.``when I move a node up into a container, it enters at the end``() =
                withLayout tree {
                    return! move Up (byName "Firefox") 
                    return! move Up (byName "Firefox")
                }
                |> should beSameTreeAs (mkTree <| C [W "Rider"; C [W "Discord"; W "Telegram"; W "Firefox";]; W "Explorer"])
                
            [<Test>]
            member x.``when I swap a node between containers, their relative positions are exchanged`` () =
                withLayout tree {
                    return! swap Up (byName "Firefox")
                }
                |> should beSameTreeAs (mkTree <| C [W "Rider"; C [W "Discord"; W "Firefox"]; C [W "Telegram"; W "Explorer"]])
                
            [<Test>]
            member x.``when I swap a node down inside a container, their relative positions are exchanged`` () =
                withLayout tree {
                    return! swap Down (byName "Firefox")
                }
                |> should beSameTreeAs (mkTree <| C [W "Rider"; C [W "Discord"; W "Telegram"]; C [W "Explorer"; W "Firefox"]])
                
            [<Test>]
            member x.``when I swap a node repeatedly, their relative order is maintained`` () =
                withLayout tree {
                    return! swap Up (byName "Firefox")
                    return! swap Up (byName "Firefox")
                }
                |> should beSameTreeAs (mkTree <| C [W "Rider"; C [W "Firefox"; W "Discord"]; C [W "Telegram"; W "Explorer"]])
                
            [<Test>]
            member x.``when I swap a node as far as it can go, their relative order is maintained`` () =
                withLayout tree {
                    return! swap Up (byName "Firefox")
                    return! swap Up (byName "Firefox")
                    return! swap Up (byName "Firefox")
                }
                |> should beSameTreeAs (mkTree <| C [W "Firefox"; C [W "Rider"; W "Discord"]; C [W "Telegram"; W "Explorer"]])
                
            [<Test>]
            member x.``when I swap a node, the weights are also swapped`` () =
                withLayout tree {
                    return! Weights.setWeight (byName "Telegram") (Weight.create 2.0 2.0)
                    return! swap Up (byName "Firefox")
                    
                    let! ffNode = find (byName "Firefox")
                    Weights.weight ffNode |> should equal (Weight.create 2.0 2.0)
                    
                    return! Some 
                }
                |> should beSameTreeAs
                       (withLayout tree {
                            return! swap Up (byName "Firefox")
                            return! Weights.setWeight (byName "Firefox") (Weight.create 2.0 2.0)
                        } |> Option.get)
               
               
        [<TestFixture; Category "move">]
        type ``Given a tree with a container in first position``() =
            let tree = mkContainer [mkContainer [mkWindow "Firefox"; mkWindow "Explorer"]; mkWindow "Rider"]
            
            [<Test>]
            member x.``when I move a window inside the nested container up it takes first position and destroys the prior parent``() =
                withLayout tree {
                    return! move Up <| byName "Firefox"
                }
                |> should beSameTreeAs (mkTree <| C [W "Firefox"; W "Explorer"; W "Rider"])
                
        [<TestFixture; Category "move">]
        type ``Given a tree with a container with one child`` () =
            let tree = mkContainer [mkContainer [mkWindow "Explorer"]; mkWindow "Rider"]
            
            [<Test>]
            member x.``when I move the last window out of a container, the container is destroyed`` () =
                withLayout tree {
                    return! move Up <| byName "Explorer"
                }
                |> should beSameTreeAs (mkTree <| C [W "Explorer"; W "Rider"])
            