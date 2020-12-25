namespace Twime
open NUnit.Framework
open Twime.LayoutTree
open Twime.TreeNavigation
open Twime.BasicNodeReferences

module TreeManipulation =
    type AddPosition = T list -> T -> T list
    
    let replaceNode (nodeRef: NodeReference) (transform: T -> T) (tree: T) =
        let rec _replaceNode tree =
            match tree with
            | WindowNode (_, _) -> if (nodeRef tree) then transform tree else tree
            | ContainerNode (ref, c, n) -> if (nodeRef tree) then transform tree else ContainerNode(ref, c, List.map _replaceNode n)
            
        match find nodeRef tree with
        | Some _ -> Some (_replaceNode tree)
        | None -> None
        

    let private addChildToNode (node: T) (child: T) (addTo: AddPosition)=
        match node with
        | WindowNode _ -> raise (TwimeError "Window objects cannot have children.")
        | ContainerNode (ref, c, nodes) -> ContainerNode(ref, c, addTo nodes child)
        
    let private transformNodeChildren (isTargetNode: NodeReference) (transformer: T list -> T list) (tree: T) =
        let _transformNodeChildren treePiece =
            match treePiece with
            | ContainerNode(ref, c, nodes) -> ContainerNode(ref, c, transformer nodes)
            | _ -> treePiece
            
        withLayout tree {
            return! replaceNode isTargetNode _transformNodeChildren
        }
        
    let addChild (addTo: AddPosition) (finder: NodeReference) (child: T) (tree: T) =
        withLayout tree {
            return! replaceNode finder (fun n -> addChildToNode n (child) addTo)
        }
        
    let toStart nodes node = node :: nodes
    let toEnd nodes node = nodes @ [node]
    
    let atIndex n nodes node =
        nodes
        |> List.splitAt n
        |> fun (before, after) -> List.concat [before; [node]; after]
        
    let beforeNode (finder: NodeReference) nodes node =
        let index = List.findIndex finder nodes
        
        atIndex index nodes node
        
    let afterNode (finder: NodeReference) nodes node =
        let index = List.findIndex finder nodes
        
        atIndex (index + 1) nodes node
    
    let deleteEmptyContainers tree =
        let rec _deleteEmptyContainers t =
            match t with
            | WindowNode wn -> Some (WindowNode wn)
            | ContainerNode (ref, contInfo, children) when LayoutTree.windows t |> List.isEmpty |> not ->
                Some (ContainerNode (ref, contInfo, List.map _deleteEmptyContainers children |> List.collect Option.toList))
            | _ -> None
               
        match tree with
        | WindowNode wn -> Some (WindowNode wn)
        | ContainerNode (ref, contInfo, children) ->
            Some (ContainerNode (ref, contInfo, List.map _deleteEmptyContainers children |> List.collect Option.toList))
    
    let deleteContainerIfSingleChild containerRef tree =
        withLayout tree {
            let! container = find containerRef
            
            let transient =
                container
                |> LayoutTree.containerDefinition
                |> Option.map Container.transient
                |> Option.defaultValue true
            
            let descendants =
                children container
            
            let descendantCount =
                descendants |> List.length
            
            if descendantCount = 1 && transient then
                return! replaceNode containerRef (fun _ -> List.head descendants)
        } |> Option.orElse (Some tree)
        
    let removeNode (node: NodeReference) (tree: T) =
        withLayout tree {
            return! transformNodeChildren (byChild node) (List.filter (fun n -> not <| node n))
            return! deleteEmptyContainers
        }
        
    let modifyWindow (node: NodeReference) (f: Window.T -> Window.T) (tree: T) =
        let apply f n =
            match n with
            | WindowNode (ref, winInfo) -> WindowNode (ref, f winInfo)
            | ContainerNode (ref, contInfo, children) -> failwith "Attempted to modify container with window mutation"
            
        withLayout tree {
            return! replaceNode node (apply f)
        }
        
    let modifyContainer (node: NodeReference) (f: Container.T -> Container.T) (tree: T) =
        let apply f n =
            match n with
            | WindowNode (ref, winInfo) -> failwith "Attempted to modify window with container modification"
            | ContainerNode (ref, contInfo, children) -> ContainerNode (ref, f contInfo, children)
            
        withLayout tree {
            return! replaceNode node (apply f)
        }
        
    module Tests =
        open Twime.LayoutTree.Tests
        open BeSameTreeAs
        open FsUnit
        
        [<TestFixture; Category "manipulation">]
        type ``Given a tree with only an empty container``() =
            let treeWithEmptyContainer = mkTree <| C []

            [<Test>]
            member x.``when I add a child to it then it has one child``() =
                addChild toEnd rootNode (mkWindow "Firefox") treeWithEmptyContainer
                |> should beSameTreeAs (mkTree (C [W "Firefox"]))
                
            [<Test>]
            member x.``when I repeatedly add children the order is correct``() =
                withLayout treeWithEmptyContainer {
                    return! addChild toEnd rootNode (mkWindow "Firefox")
                    return! addChild toEnd rootNode (mkWindow "Firefox 2")
                    return! addChild toStart rootNode (mkTree (C [W "Firefox 3"; W "Firefox 4"]))
                }
                |> should beSameTreeAs (
                     mkTree (C [
                        C [W "Firefox 3"; W "Firefox 4"];
                        W "Firefox"
                        W "Firefox 2"
                    ]))
                
            [<Test>]
            member x.``when I delete empty trees it is not deleted`` () =
                deleteEmptyContainers treeWithEmptyContainer
                |> should beSameTreeAs treeWithEmptyContainer
                
        [<TestFixture; Category "manipulation">]
        type ``Given a tree with empty nested containers`` () =
            let tree = mkTree <| C [W "Rider"; C []]
            
            [<Test>]
            member x.``when I delete the empty containers, it is destroyed`` () =
                deleteEmptyContainers tree
                |> should beSameTreeAs (mkTree <| C [W "Rider"])
            
                
        [<TestFixture; Category "manipulation">]
        type ``Given a tree with nested containers``() =
            let tree = mkTree <| C [W "Rider"; C [W "Firefox"; W "Explorer"]]
            
            [<Test>]
            member x.``when I add a child to the first container it has three children``() =
                addChild toEnd rootNode (mkWindow "Notepad") tree
                |> should beSameTreeAs (mkTree (C [W "Rider"; C [W "Firefox"; W "Explorer"]; W "Notepad"]))
                
            [<Test>]
            member x.``when I add a child to the nested container it has three children``() =
                addChild toEnd (byChild (byName "Firefox")) (mkWindow "Notepad") tree 
                |> should beSameTreeAs (
                     mkTree (C
                                 [W "Rider"
                                  C
                                      [W "Firefox"
                                       W "Explorer"
                                       W "Notepad"]]))
                
            [<Test>]
            member x.``when I add a child to a particular index it appears there``() =
                addChild (atIndex 1) rootNode (mkWindow "Notepad") tree 
                |> should beSameTreeAs (mkTree (C [W "Rider"; W "Notepad"; C [W "Firefox"; W "Explorer"]]))
                
                
            [<Test>]
            member x.``when I remove a child it is gone``() =
                removeNode (byName "Firefox") tree
                |> should beSameTreeAs (mkTree (C [W "Rider"; C [W "Explorer"]]))
                
            [<Test>]
            member x.``when I rename a node its name changes`` () =
                modifyWindow (byName "Firefox") (fun w -> {w with Name="Firefix"; Definition = {w.Definition with title="Firefix"}}) tree
                |> should beSameTreeAs (mkTree (C [W "Rider"; C [W "Firefix"; W "Explorer"]]))
