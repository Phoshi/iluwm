namespace Twime

open Maybe
    
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
            (TreeReference.create (), Window.create (def.title) def)
            
    let container cont children =
        ContainerNode
            (TreeReference.create (), cont, children)
            
    let rec clone (t: T) =
        match t with
        | WindowNode (_, w) -> WindowNode (TreeReference.create(), w)
        | ContainerNode (_, c, n) -> ContainerNode (TreeReference.create (), c, n |> List.map clone)
            
    let rec windowNodes (layout: T) =
        match layout with
        | WindowNode (_, _) -> [layout]
        | ContainerNode (_, _, children) -> List.collect windowNodes children
         
    let isContainer layout =
        match layout with
        | ContainerNode _ -> true
        | _ -> false
        
    let isWindow layout =
        match layout with
        | WindowNode _ -> true
        | _ -> false
    
    let rec windows (layout: T) =
        match layout with
        | WindowNode (_, w) -> [w]
        | ContainerNode (_, _, children) -> List.collect windows children
        
    let containerDefinition t =
        match t with
        | WindowNode _ -> None
        | ContainerNode (_, ci, _) -> Some ci
        
    let windowDefinition t =
        match t with
        | WindowNode (_, wi) -> Some wi
        | ContainerNode _ -> None
    
    let mapContainerDefinition f t =
        containerDefinition t
        |> Option.map f
        
    let layoutEngine (layout: T) =
        match layout with
        | WindowNode _ -> "horizontal"
        | ContainerNode (_, ci, _) -> ci.LayoutEngine
        
    let isSelected (layout: T) =
        match layout with
        | WindowNode (_, w) -> w.Definition.selected
        | ContainerNode (_, c, _) -> c.Selected
        
    let selected s t =
        match t with
        | WindowNode (r, w) -> WindowNode (r, Window.withDefinition (Window.Definition.withSelected s) w)
        | ContainerNode (r, c, children) -> ContainerNode (r, Container.withSelected s c, children)
        
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
        
    let apply transformations layout =
        let mutable l = Some layout
        for trans in transformations do
            l <- Option.bind trans l
            
        l
        
    type LayoutMutator = T -> T option
    type LayoutBuilder(tree: T) =
        let mutable _tree = Some tree
        member this.Bind(f, continueWith) =
            maybe {
               let! tree = _tree
               let! expressionResult = f tree
               
               return! continueWith expressionResult
            }
        member this.Combine(_, b) =
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
        let _mkWindow nextRef name active minSize = WindowNode (nextRef(), {Name = name; Definition = Window.Definition.create Box.zero Weight.init name "" false false active (WindowHandle.none) |> Window.Definition.withMinSize minSize})
        let mkWindow name = _mkWindow TreeReference.creator name false Box.zero
        
        type TestTree =
            | W of string
            | Wmin of (string * Box.T)
            | A of string
            | C of TestTree list
            | V of TestTree list
        let rec _mkTree nextRef (tree: TestTree) =
            match tree with
            | W s -> _mkWindow nextRef s false Box.zero
            | Wmin (s, b) -> _mkWindow nextRef s false b
            | A s -> _mkWindow nextRef s true Box.zero
            | C tl -> _mkContainer nextRef (List.map (_mkTree nextRef) tl)
            | V tl -> _mkVContainer nextRef (List.map (_mkTree nextRef) tl)
            
        let mkTree tree = _mkTree TreeReference.creator tree
        
