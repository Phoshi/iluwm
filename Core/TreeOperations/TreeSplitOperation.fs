namespace Twime

open NUnit.Framework
open BasicNodeReferences
open TreeNavigation
open TreeManipulation
open LayoutTree
open TreeOperation

module TreeSplitOperation =
    let isContainer ref tree =
        let getContainer layout =
            withLayout layout {
                let! node = find ref
                if node |> containerDefinition |> Option.isSome then
                    return! Some
            }
            
        Tree.mapLayout
            (exists bySelection)
            getContainer
            tree
        |> Option.isSome
            
            
    let containerWithLayout engine (container: T) =
        T.ContainerNode
            (ref container,
             mapContainerDefinition (Container.withEngine engine) container
             |> Option.defaultValue (Container.create engine),
             children container)
    
    let nextInList current list =
        let indexInRotation = List.tryFindIndex ((=)current) list
        let newIndex =
            indexInRotation
            |> Option.map (fun i -> (i+1) % (List.length list))
            |> Option.defaultValue 0
        List.item newIndex list
        
            
    let containerWithNextLayout engines (container: T) =
        let currentLayout =
            layoutEngine container
        let newEngine = nextInList currentLayout engines
            
        T.ContainerNode
            (ref container,
             mapContainerDefinition (Container.withEngine newEngine) container
             |> Option.defaultValue (Container.create newEngine),
             children container)
            
    let windowToContainer direction finder (container: T) =
        let _replaceWindowWithContainer direction finder window =
            (T.ContainerNode
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
        
    let windowToContainerWithNextLayout directions finder (container: T) =
        let _replaceWindowWithContainer direction finder window =
            (T.ContainerNode
                (TreeReference.create (),
                 Container.create direction,
                 [window]))
            
        withLayout container {
            let! window = find finder
            let children = children container
            let currentEngine = layoutEngine container
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
                 (replaceNode (byChild byActiveWindow) (windowToContainer direction byActiveWindow)))
            
    let splitActiveWindowRotate directions =
        Tree.mapDisplay
            hasActiveWindow
            (Display.mapActiveLayout
                 (replaceNode (byChild byActiveWindow) (windowToContainerWithNextLayout directions byActiveWindow)))
            
    let setActiveContainerLayoutEngine engine =
        Tree.mapDisplay
            hasActiveWindow
            (Display.mapActiveLayout
                (replaceNode (byChild byActiveWindow) (containerWithLayout engine)))
            
    let rotateLayoutEngine engines ref =
        Tree.mapDisplay
            (hasWindow ref)
            (Display.mapActiveLayout
                (replaceNode ref (containerWithNextLayout engines)))
    
    let rotateActiveContainerLayoutEngine engines =
        rotateLayoutEngine engines (byChild byLastActiveWindow)
        
    let splitSelection direction tree =
        let selector =
            if isContainer bySelection tree then bySelection else byChild bySelection
            
        Tree.mapDisplay
            hasActiveWindow
            (Display.mapActiveLayout
                 (replaceNode selector (windowToContainer direction bySelection)))
            tree
            
    let splitSelectionRotate directions tree =
        let selector =
            if isContainer bySelection tree then bySelection else byChild bySelection
            
        Tree.mapDisplay
            hasActiveWindow
            (Display.mapActiveLayout
                 (replaceNode selector (windowToContainerWithNextLayout directions bySelection)))
            tree
            
    let setSelectionLayoutEngine engine tree =
        let selector =
            if isContainer bySelection tree then bySelection else byChild bySelection
        Tree.mapDisplay
            hasActiveWindow
            (Display.mapActiveLayout
                (replaceNode selector (containerWithLayout engine)))
            tree
            
    let rotateSelectionLayoutEngine engines tree =
        let selector =
            if isContainer bySelection tree then bySelection else byChild bySelection
        rotateLayoutEngine engines selector tree

    module Tests =
        open Twime.LayoutTree.Tests
        open BeSameTreeAs
        open FsUnit
        let layoutToTree layout =
            layout
            |> Tag.create (TreeReference.create ()) (Tag.createMeta "1" "1" None None GapConfig.none)
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
