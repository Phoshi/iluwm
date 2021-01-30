namespace Twime

open NUnit.Framework
open LayoutTree
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
        
    let layout uiSize ui boundsize tree =
        _layout uiSize ui boundsize (byExactNode tree) tree
        
    module Tests =
        open Twime.LayoutTree.Tests
        open FsUnit
        
        let winWithWeight name weight size =
            LayoutPiece.Window
                (Window.create
                     name
                     (Window.Definition.create Box.zero weight name "" false false false WindowHandle.none),
                     
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
