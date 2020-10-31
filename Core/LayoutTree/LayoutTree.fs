namespace Twime

open Maybe
open NUnit.Framework.Constraints
open Twime.Window
    
module LayoutTree =
    type T =
        | WindowNode of WindowNode
        | ContainerNode of ContainerNode
    and WindowNode = TreeReference.T * Window.T
    and ContainerNode = TreeReference.T * Container.T * T list
    
    let ref t =
        match t with
        | WindowNode (ref, _) -> ref
        | ContainerNode (ref, _, _) -> ref
    
    let window (def: Window.Definition.T) =
        WindowNode
            (TreeReference.create (), Window.create (def.title) Weight.init def)
            
    let rec clone (t: T) =
        match t with
        | WindowNode (_, w) -> WindowNode (TreeReference.create(), w)
        | ContainerNode (_, c, n) -> ContainerNode (TreeReference.create (), c, n |> List.map clone)
            
    let rec windowNodes (layout: T) =
        match layout with
        | WindowNode (_, w) -> [layout]
        | ContainerNode (_, _, children) -> List.collect windowNodes children
         
    let rec windows (layout: T) =
        match layout with
        | WindowNode (_, w) -> [w]
        | ContainerNode (_, _, children) -> List.collect windows children
        
    let containerDefinition t =
        match t with
        | WindowNode _ -> None
        | ContainerNode (_, ci, _) -> Some ci
    
    let mapContainerDefinition f t =
        containerDefinition t
        |> Option.map f
        
    let layoutEngine (layout: T) =
        match layout with
        | WindowNode _ -> "horizontal"
        | ContainerNode (_, ci, _) -> ci.LayoutEngine
        
    let rec lastActive tree =
        match tree with
        | WindowNode _ -> Some (ref tree)
        | ContainerNode (_, c, ch) ->
            Container.lastActiveChild c
            |> Option.bind (fun r ->
                ch
                |> List.tryFind (fun child -> (ref child = r))
                |> Option.bind lastActive
                )
        
    let rec cataTree fWindow fContainer t =
        let recurse = cataTree fWindow fContainer
        
        match t with
        | WindowNode wn -> fWindow wn
        | ContainerNode (ref, container, children) ->
            let childs =
                children
                |> List.map recurse
            fContainer (ref, container, childs)
        
    type LayoutMutator = T -> T option
    type LayoutBuilder(tree: T) =
        let mutable _tree = Some tree
        member this.Bind(f, continueWith) =
            maybe {
               let! tree = _tree
               let! expressionResult = f tree
               
               return! continueWith expressionResult
            }
        member this.Combine(a, b) =
            b
        member this.Delay(f) = f()
        member this.Zero() = None
        member this.Return(x) = x
        member this.ReturnFrom(x: LayoutMutator) =
            _tree <-
                match _tree with
                | Some t -> x t
                | None -> None
            _tree
            
    let withLayout tree = LayoutBuilder(tree)
                                  
    module Tests =
        let _mkContainer nextRef nodes = ContainerNode((nextRef(), Container.create "horizontal", nodes))
        let _mkVContainer nextRef nodes = ContainerNode((nextRef(), Container.create "vertical", nodes))
        let mkContainer nodes = _mkContainer TreeReference.creator nodes
        let mkVContainer nodes = _mkVContainer TreeReference.creator nodes
        let _mkWindow nextRef name active = WindowNode (nextRef(), {Name = name; Weight = Weight.init; Definition = Window.Definition.create Box.zero name false false active (WindowHandle.none)})
        let mkWindow name = _mkWindow TreeReference.creator name false
        
        type TestTree =
            | W of string
            | A of string
            | C of TestTree list
            | V of TestTree list
        let rec _mkTree nextRef (tree: TestTree) =
            match tree with
            | W s -> _mkWindow nextRef s false
            | A s -> _mkWindow nextRef s true
            | C tl -> _mkContainer nextRef (List.map (_mkTree nextRef) tl)
            | V tl -> _mkVContainer nextRef (List.map (_mkTree nextRef) tl)
            
        let mkTree tree = _mkTree TreeReference.creator tree
        
