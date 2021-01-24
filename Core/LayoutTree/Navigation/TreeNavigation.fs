namespace Twime

open Twime.LayoutTree
open NUnit.Framework

module TreeNavigation =
    type NodeReference = LayoutTree.T -> bool
    
    let children (node: T) =
        match node with
        | WindowNode _ -> []
        | ContainerNode (_, _, nodes) -> nodes
        
    let rec parent (node: T) (tree: T) =
        match tree with
        | WindowNode _ ->
            None
        | ContainerNode (_, _, nodes) when List.contains node nodes ->
            Some tree
        | ContainerNode (_, _, nodes) ->
            List.map (parent node) nodes
            |> List.filter Option.isSome
            |> fun l -> if (List.length l = 1) then l.Head else None
                                  
    let rec find (nodeRef: NodeReference) (tree: T) =
        match tree with
        | _ when nodeRef tree -> Some tree
        | _ -> List.map (find nodeRef) (children tree)
                |> List.filter Option.isSome
                |> fun l -> if (List.length l >= 1) then l.Head else None
        | _ when List.isEmpty (children tree) -> None
        
    let exists (finder: NodeReference) (tree: T) =
        match find finder tree with
        | Some _ -> true
        | None -> false
                                  

    module Tests =
        open FsUnit
        open Twime.LayoutTree.Tests
        
        [<TestFixture; Category "navigation">]
        type ``Given a tree with nested containers``() =
            let tree = mkTree (C [W "Rider"; C [W "Firefox"; W "Explorer"]])
            
            [<Test>]
            member x.``it has two children``() =
                children tree
                |> List.length
                |> should equal 2
        
        [<TestFixture; Category "navigation">]
        type ``Given a tree with only an empty container``() =
            let treeWithEmptyContainer = mkTree <| C []
            let rootOfTree = treeWithEmptyContainer

            [<Test>]
            member x.``it does not have children``() =
                children rootOfTree
                |> should equal []
                
            [<Test>]
            member x.``it has no parent``() =
                parent rootOfTree treeWithEmptyContainer
                |> should equal None
