namespace Twime

open NUnit.Framework
open LayoutTree
open NUnit.Framework.Internal
open Twime.LayoutEngines
open Twime.LayoutUIComponents
open Twime.TreeNavigation
open Twime.BasicNodeReferences
open Twime.Window

module Layout =
    type Engine =
        | Horizontal 
        | Vertical
        | Spiral
        
    let engine uiSize ui boundSize containerRef tree =
        withLayout tree {
            let! container = find containerRef
            return match container with
                        | ContainerNode (_, ci, _) ->
                            match ci.LayoutEngine with
                            | "horizontal" -> Some (horizontalLayout uiSize boundSize containerRef, ui uiSize boundSize containerRef)
                            | "vertical" -> Some (verticalLayout uiSize boundSize containerRef, ui uiSize boundSize containerRef)
                            | "tabbed" -> Some (tabbedLayout uiSize boundSize containerRef, ui uiSize boundSize containerRef)
                            | "sidebar-right" -> Some (verticalLayout 0 boundSize containerRef, noUi)
                            | "sidebar-left" -> Some (verticalLayout 0 boundSize containerRef, noUi)
                            | _ -> None
                        | _ -> None
        }
        
    type LayoutPiece =
        | Window of Window.T * Box.T
        | Container of Container.T * Box.T
        | UI of UIComponent.T list
        
    let rec _layout uiSize ui boundSize containerRef tree : LayoutPiece list option =
        withLayout tree {
            let! (layoutEngine, containerUi) = engine uiSize ui boundSize containerRef
            let! container = find containerRef
            let uis = [UI (containerUi tree)]
                
            let layout = seq {
                for (i, child) in (TreeNavigation.children container |> List.indexed)
                 ->
                     match child with
                     | WindowNode (_, wi) ->
                         let (box) = layoutEngine i tree |> Option.get
                         Some [Window (wi, box)]
                     | ContainerNode (_, ci, _) ->
                         let (box) = layoutEngine i tree |> Option.get
                         _layout uiSize ui box (byExactNode child) tree
                         |> Option.map (fun pieces -> Container (ci, box) :: pieces)
            }
            
            return layout
                   |> Seq.map Option.toList
                   |> Seq.map List.concat
                   |> List.concat
                   |> Seq.toList
                   |> List.append uis
                   |> Some
        }
        
    let directionFor engine =
        match engine with
        | "horizontal" -> Box.width
        | "vertical" -> Box.height
        | _ -> Box.width
            
    let rec minimumSize containerEngine (node: LayoutTree.T) =
            
        match node with
        | WindowNode (_, wi) -> wi.Definition.minSize |> (directionFor containerEngine)
        | ContainerNode (_, ci, children) ->
            if ci.LayoutEngine = containerEngine then
                children
                |> List.map (minimumSize containerEngine)
                |> List.sum
            else
                children
                |> List.map (minimumSize containerEngine)
                |> List.max
    let isUndersized (window: LayoutPiece) =
        match window with
        | LayoutPiece.Window (wi, box) ->
            let minSize = wi.Definition.minSize
            (Box.width minSize) > (Box.width box) || (Box.height minSize) > (Box.height box)
        | _ -> false
        
    let hasUndersizedWindows (windows: LayoutPiece list) =
        windows
        |> List.exists isUndersized
        
    let doubleWeightForUndersized (windows: LayoutPiece list) scalingFactor (tree: LayoutTree.T)  =
        let windowRef lp =
            match lp with
            | LayoutPiece.Window (w, _) -> (byWindow w)
            | _ -> failwith "Tried to get the window of a nonwindow layout piece"
            
        let scaleFactor lp baseFactor =
            match lp with
            | LayoutPiece.Window (w, b) ->
                let hScaling =
                    if (Box.width w.Definition.minSize) > (Box.width b) then baseFactor else 1.0f
                let vScaling =
                    if (Box.height w.Definition.minSize) > (Box.height b) then baseFactor else 1.0f
                (hScaling, vScaling)
            
        let scaleWeight ref (hfactor, vfactor) tree =
            Weights.adjustWeight ref (fun w -> w |> Weight.hmultiply hfactor |> Weight.vmultiply vfactor) tree
        let mutable t = Some tree
        
        let undersized =
            windows
            |> List.where isUndersized
            
        for window in undersized do
            t <- Option.bind (scaleWeight (windowRef window) (scaleFactor window scalingFactor)) t
            
        t
        
        
        
    let rec _layoutWithMinimums uiSize ui boundSize containerRef i tree : LayoutPiece list option =
        let computedLayout = _layout uiSize ui boundSize (byExactNode tree) tree |> Option.get
        if hasUndersizedWindows computedLayout && i < 5.0f then
            let newTree = doubleWeightForUndersized computedLayout i tree |> Option.get
            _layoutWithMinimums uiSize ui boundSize containerRef (i+0.25f) newTree
        else
            Some computedLayout
        
    let layout uiSize ui boundsize tree =
        _layoutWithMinimums uiSize ui boundsize (byExactNode tree) 1.25f tree
        
    module Tests =
        open Twime.LayoutTree.Tests
        open FsUnit
        
        let winWithWeight name weight size =
            LayoutPiece.Window
                (Window.create
                     name
                     (Window.Definition.create Box.zero weight Box.zero name "" false false false WindowHandle.none),
                     
                     size
                )
        let winWithMin name min size =
            LayoutPiece.Window
                (Window.create
                     name
                     (Window.Definition.create Box.zero Weight.init min name "" false false false WindowHandle.none),
                     
                     size
                )
        let win name size = winWithWeight name Weight.init size
        
        let noUi _ _ _ _ = []
        
        let layout = _layout 0 noUi
        
        let removeUis (layout: LayoutPiece list) =
            let isWindow l =
                match l with
                 | LayoutPiece.Window _ -> true
                 | _ -> false
                 
            layout
            |> List.where isWindow
                
        
        [<TestFixture; Category "layout">]
        type ``Given a horizontal tree with one container and three windows`` () =
            let tree = mkTree <| C [W "Firefox"; W "Rider"; W "Notepad"]
            
            [<Test>]
            member x.``each window gets a third of the space``() =
                tree
                |> layout (Box.create 0 0 99 100) (byExactNode tree)
                |> Option.map removeUis
                |> should equal (Some ([
                    win "Firefox" <| Box.create 0 0 33 100
                    win "Rider" <| Box.create 33 0 66 100
                    win "Notepad" <| Box.create 66 0 99 100
                ]))
                
            [<Test>]
            member x.``no pixels are wasted if sizes don't evenly divide``() =
                tree
                |> layout (Box.create 0 0 100 100) (byExactNode tree)
                |> Option.map removeUis
                |> should equal (Some ([
                    win "Firefox" <| Box.create 0 0 33 100
                    win "Rider" <| Box.create 33 0 66 100
                    win "Notepad" <| Box.create 66 0 100 100
                ]))
                
        [<TestFixture; Category "layout">]
        type ``Given a horizontal tree with two windows and a vertical container with two windows``() =
            let tree = mkTree <| C [W "Firefox"
                                    W "Rider"
                                    V [
                                        W "Notepad"
                                        W "Discord"
                                    ]
                                    ]
            
            [<Test>]
            member x.``the first two windows get two thirds, then the last two are tiled vertically in the last third``() =
                let laidOut = tree
                                |> layout (Box.create 0 0 100 100) (byExactNode tree)
                                |> Option.map removeUis
                                
                laidOut |> should equal (Some ([
                   win "Firefox" <| Box.create 0 0 33 100
                   win "Rider" <| Box.create 33 0 66 100
                   win "Notepad" <| Box.create 66 0 100 50
                   win "Discord" <| Box.create 66 50 100 100
                ]))
                
        [<TestFixture; Category "layout">]
        type ``Given a tree with deeply nested containers``() =
            let tree = mkTree <| C [
                W "Firefox"
                V [
                    W "Notepad"
                    C [
                        W "Discord"
                        W "Rider"
                    ]
                ]
            ]
            
            [<Test>]
            member x.``the nested layout lays out correctly``() =
                tree
                |> layout (Box.create 0 0 100 100) (byExactNode tree)
                |> Option.map removeUis
                |> should equal (Some ([
                    win "Firefox" <| Box.create 0 0 50 100
                    win "Notepad" <| Box.create 50 0 100 50
                    win "Discord" <| Box.create 50 50 75 100
                    win "Rider" <| Box.create 75 50 100 100
                ]))
                
            [<Test>]
            member x.``horizontal weights increase horizontal space`` () =
                withLayout tree {
                    return! Weights.setWeight (byName "Firefox") (Weight.create 3.0 1.0)
                }
                |> Option.get
                |> layout (Box.create 0 0 100 100) (byExactNode tree)
                |> Option.map removeUis
                |> should equal (Some ([
                    winWithWeight "Firefox" (Weight.create 3.0 1.0) <| Box.create 0 0 75 100
                    win "Notepad" <| Box.create 75 0 100 50
                    win "Discord" <| Box.create 75 50 87 100
                    win "Rider" <| Box.create 87 50 100 100
                ]))
                
            [<Test>]
            member x.``horizontal weights in nested containers increase horizontal space`` () =
                withLayout tree {
                    return! Weights.setWeight (byName "Notepad") (Weight.create 3.0 1.0)
                }
                |> Option.get
                |> layout (Box.create 0 0 100 100) (byExactNode tree)
                |> Option.map removeUis
                |> should equal (Some ([
                    win "Firefox" <| Box.create 0 0 25 100
                    winWithWeight "Notepad" (Weight.create 3.0 1.0) <| Box.create 25 0 100 50
                    win "Discord" <| Box.create 25 50 62 100
                    win "Rider" <| Box.create 62 50 100 100
                ]))
                
            [<Test>]
            member x.``complex weights in nested containers increase space`` () =
                withLayout tree {
                    return! Weights.setWeight (byName "Rider") (Weight.create 2.0 2.0)
                }
                |> Option.get
                |> layout (Box.create 0 0 100 100) (byExactNode tree)
                |> Option.map removeUis
                |> should equal (Some ([
                    win "Firefox" <| Box.create 0 0 33 100
                    win "Notepad" <| Box.create 33 0 100 33
                    win "Discord" <| Box.create 33 33 55 100
                    winWithWeight "Rider" (Weight.create 2.0 2.0) <| Box.create 55 33 100 100
                ]))
                
        [<TestFixture; Category "layout">]
        type ``Given a tree with deeply nested containers and minimum sizes``() =
            let tree = mkTree <| C [
                W "Firefox"
                V [
                    W "Notepad"
                    C [
                        W "Discord"
                        Wmin ("Rider", Box.create 0 0 50 50)
                    ]
                ]
            ]
            
            let layout = _layoutWithMinimums 0 noUi
            
            [<Test>]
            member x.``minimum sizes in nested containers are respected`` () =
                let rendered =
                    tree
                    |> layout (Box.create 0 0 100 100) (byExactNode tree) 1f
                    |> Option.map removeUis
                    |> Option.get
                    
                let window =
                    rendered
                    |> List.item 3
                    
                match window with
                | LayoutPiece.Window (wi, bo) ->
                    ((Box.width wi.Definition.minSize) <= (Box.width bo) &&
                    (Box.height wi.Definition.minSize) <= (Box.height bo))
                    |> should be True
                    
            [<Test>]
            member x.``window sizes aren't increased in both dimensions when only one needs to grow`` () =
                let rendered =
                    tree
                    |> layout (Box.create 0 0 100 100) (byExactNode tree) 1f
                    |> Option.map removeUis
                    |> Option.get
                    
                let window =
                    rendered
                    |> List.item 3
                    
                match window with
                | LayoutPiece.Window (_, bo) ->
                    (Box.height bo)
                    |> should equal 50
                    
                
