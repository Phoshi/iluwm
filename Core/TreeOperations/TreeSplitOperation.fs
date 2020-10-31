namespace Twime

open NUnit.Framework
open BasicNodeReferences
open TreeNavigation
open TreeManipulation
open LayoutTree
open TreeOperation

module TreeSplitOperation =
    let containerWithLayout engine (container: LayoutTree.T) =
        LayoutTree.T.ContainerNode
            (LayoutTree.ref container,
             LayoutTree.mapContainerDefinition (Container.withEngine engine) container
             |> Option.defaultValue (Container.create engine),
             children container)
    
    let nextInList current list =
        let indexInRotation = List.tryFindIndex ((=)current) list
        let newIndex =
            indexInRotation
            |> Option.map (fun i -> (i+1) % (List.length list))
            |> Option.defaultValue 0
        List.item newIndex list
        
            
    let containerWithNextLayout engines (container: LayoutTree.T) =
        let currentLayout =
            LayoutTree.layoutEngine container
        let newEngine = nextInList currentLayout engines
            
        LayoutTree.T.ContainerNode
            (LayoutTree.ref container,
             LayoutTree.mapContainerDefinition (Container.withEngine newEngine) container
             |> Option.defaultValue (Container.create newEngine),
             children container)
            
    let windowToContainer direction finder (container: LayoutTree.T) =
        let _replaceWindowWithContainer direction finder window =
            (LayoutTree.T.ContainerNode
                (TreeReference.create (),
                 Container.create direction,
                 [window]))
            
        withLayout container {
            let! window = find finder
            let children = children container
            if List.length children > 1 then
                return! replaceNode finder (_replaceWindowWithContainer direction finder)
            else
                return (Some (_replaceWindowWithContainer direction finder window))
                
        }
        |> Option.get
        
    let windowToContainerWithNextLayout directions finder (container: LayoutTree.T) =
        let _replaceWindowWithContainer direction finder window =
            (LayoutTree.T.ContainerNode
                (TreeReference.create (),
                 Container.create direction,
                 [window]))
            
        withLayout container {
            let! window = find finder
            let children = children container
            let currentEngine = LayoutTree.layoutEngine container
            let engine = nextInList currentEngine directions
            if List.length children > 1 then
                return! replaceNode finder (_replaceWindowWithContainer engine finder)
            else
                return (Some (_replaceWindowWithContainer engine finder window))
                
        }
        |> Option.get
            
    let splitActiveWindow direction =
        Tree.mapDisplay
            hasActiveWindow
            (Display.mapActiveLayout
                 (TreeManipulation.replaceNode (byChild byActiveWindow) (windowToContainer direction byActiveWindow)))
            
    let splitActiveWindowRotate directions =
        Tree.mapDisplay
            hasActiveWindow
            (Display.mapActiveLayout
                 (TreeManipulation.replaceNode (byChild byActiveWindow) (windowToContainerWithNextLayout directions byActiveWindow)))
            
    let setActiveContainerLayoutEngine engine =
        Tree.mapDisplay
            hasActiveWindow
            (Display.mapActiveLayout
                (TreeManipulation.replaceNode (byChild byActiveWindow) (containerWithLayout engine)))
            
    let rotateLayoutEngine engines ref =
        Tree.mapDisplay
            (hasWindow ref)
            (Display.mapActiveLayout
                (TreeManipulation.replaceNode ref (containerWithNextLayout engines)))
    
    let rotateActiveContainerLayoutEngine engines =
        rotateLayoutEngine engines (byChild byLastActiveWindow)

    module Tests =
        open Twime.LayoutTree.Tests
        open BeSameTreeAs
        open FsUnit
        let layoutToTree layout =
            layout
            |> Tag.create (TreeReference.create ()) (Tag.createMeta "1" "1" None None)
            |> fun t -> Display.create (TreeReference.create ()) (Display.createMeta "disp" Box.zero Box.zero true false) [t] t.Reference
            |> fun d -> TwimeRoot.create [d] []
            
        let treeToLayout (tree: TwimeRoot.T option) =
            tree
            |> Option.get
            |> fun r  -> r.Displays
            |> List.collect (fun d -> d.Tags)
            |> List.map Tag.layout
            |> List.head
        
        [<TestFixture; Category "split">]
        type ``Given a layout with two windows at the root`` () =
            let root =
                C [W "Firefox"; A "Rider"]
                |> mkTree
                |> layoutToTree
                
            [<Test>]
            member x.``When I bisect the active window, the active window becomes nested in a horizontal container`` () =
                root
                |> splitActiveWindow "horizontal"
                |> treeToLayout
                |> should beSameTreeAs (
                                           C [
                                              W "Firefox"
                                              C [
                                                  A "Rider"
                                              ]
                                              ]
                                           |> mkTree
                                       )
                
            [<Test>]
            member x.``When I v-split the active window, the active window becomes nested in a vertical container`` () =
                root
                |> splitActiveWindow "vertical"
                |> treeToLayout
                |> should beSameTreeAs (
                                           C [
                                              W "Firefox"
                                              V [
                                                  A "Rider"
                                              ]
                                              ]
                                           |> mkTree
                                       )
                
        [<TestFixture; Category "split">]
        type ``Given a layout with one window and one horizontal container with one active window at the root`` () =
            let root =
                C [W "Firefox"; C [A "Rider"]]
                |> mkTree
                |> layoutToTree
                
            [<Test>]
            member x.``when I horizontally split the active window nothing happens`` () =
                root
                |> splitActiveWindow "horizontal"
                |> treeToLayout
                |> should beSameTreeAs (
                                           C [
                                              W "Firefox"
                                              C [
                                                  A "Rider"
                                              ]
                                              ]
                                           |> mkTree
                                       )
            [<Test>]
            member x.``when I vertically split the active window its parent container changes`` () =
                root
                |> splitActiveWindow "vertical"
                |> treeToLayout
                |> should beSameTreeAs (
                                           C [
                                              W "Firefox"
                                              V [
                                                  A "Rider"
                                              ]
                                              ]
                                           |> mkTree
                                       )
