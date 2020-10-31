namespace Twime

open System.Drawing
open Twime.LayoutUIComponents
open Twime.LayoutTree
open Twime.Weights
open Twime.TreeNavigation
open NUnit.Framework

module LayoutEngines =
    type LayoutResponse = (Box.T) option
    
    let noUi _ = []
    
    let child container index  =
        children container
        |> List.item index
        
    let isLastChild container index =
        List.length (children container) = index + 1
        
    let dimensionForNode (box: Box.T) (sumWeights: Weight.T) (weightKind: Weight.T -> float) (dimensionality: Box.T -> int) (node: T) =
        let percentageForWindow =
            (weightKind (weight node))
            / (weightKind sumWeights)
        
        (percentageForWindow * (dimensionality box |> float)) |> int
        
    let rec sumOfDimensionToIndex start win dimensionForNode i =
        match i with
        | 0 -> start
        | n ->
            (dimensionForNode (win (n-1))) + sumOfDimensionToIndex start win dimensionForNode (n - 1)
    
    let simpleLayoutPositioner box weightSelector dimensionSelector (startEdge: Box.T -> int) (finishEdge: Box.T -> int) containerRef index tree =
        withLayout tree {
            let! sumWeights = sumWeights containerRef
            let! container = find containerRef
            
            let win = child container
            let scalingDimensionForNode =
                dimensionForNode box sumWeights weightSelector dimensionSelector
            let startPositionForNode =
                sumOfDimensionToIndex (startEdge box) win scalingDimensionForNode
                    
            let nodeScalingDimension =
                 if isLastChild container index
                 then ((finishEdge box)) - (startPositionForNode index |> int) 
                 else (scalingDimensionForNode (win index))
                 
            return Some (startPositionForNode index |> int, nodeScalingDimension |> int)
        }
    let public horizontalLayout uiSize boundSize containerRef index tree : LayoutResponse =
        withLayout tree {
            let! (start, width) = simpleLayoutPositioner (Box.add 0 uiSize 0 0 boundSize) Weight.horizontal Box.width Box.left Box.right containerRef index
                 
            return Some (Box.create start (boundSize.Top + uiSize) (start + width) boundSize.Bottom)
        }
        
    let public verticalLayout uiSize boundSize containerRef index tree : LayoutResponse =
        withLayout tree {
            let! (start, height) = simpleLayoutPositioner (Box.add 0 uiSize 0 0 boundSize) Weight.vertical Box.height Box.top Box.bottom containerRef index
             
            return (Box.create boundSize.Left start boundSize.Right (start + height))
                    |> Some
        }
        
    type Direction =
       | Right
       | Down
       | Left
       | Up
       
    let public spiralLayout uiSize boundSize containerRef index tree : LayoutResponse =
       withLayout tree {
           let! container = find containerRef
           let childCount = children container |> List.length |> float
           let! sumWeights = Weights.sumWeights containerRef
           
           let win = child container
               
           let averageWeight =
               sumWeights
               |> (fun w -> Weight.create 
                                       (Weight.horizontal w / childCount)
                                       (Weight.vertical w / childCount)
                                   )
               
           let rec _spiralLayout bounds direction i =
               if i+1 = (container |> TreeNavigation.children |> List.length) then
                   bounds
               else
               let w = Box.width bounds
               let h = Box.height bounds
               
               let hFactor =
                   (weight (win i) |> Weight.horizontal)
                   / (Weight.horizontal averageWeight)
                   
               let vFactor =
                   (weight (win i) |> Weight.vertical)
                   / (Weight.vertical averageWeight)
                   
               let effectiveWidth = ((w |> float) / (2.0 / hFactor)) |> int
               let effectiveHeight = ((h |> float) / (2.0 / vFactor)) |> int
               
               match direction with
               | Right ->
                          if i = index then
                              Box.create bounds.Left bounds.Top (bounds.Left + effectiveWidth) bounds.Bottom
                          else
                              _spiralLayout
                                  (Box.create 
                                           (bounds.Left + effectiveWidth)
                                           bounds.Top
                                           bounds.Right
                                           bounds.Bottom
                                       ) Down (i+1)
               | Down ->
                          if i = index then
                              Box.create bounds.Left bounds.Top bounds.Right (bounds.Top + effectiveHeight)
                          else
                             _spiralLayout
                                 (Box.create 
                                          bounds.Left
                                          (bounds.Top + effectiveHeight)
                                          bounds.Right
                                          bounds.Bottom
                                      ) Left (i+1)
               | Left ->
                          if i = index then
                              Box.create (bounds.Left + effectiveWidth) bounds.Top bounds.Right bounds.Bottom
                          else
                             _spiralLayout
                                 (Box.create 
                                          bounds.Left
                                          bounds.Top
                                          (bounds.Left + effectiveWidth)
                                          bounds.Bottom
                                      ) Up (i+1)
               | Up ->
                      if i = index then
                          Box.create bounds.Left (bounds.Top + effectiveHeight) bounds.Left bounds.Bottom
                      else
                           _spiralLayout
                               (Box.create 
                                        bounds.Left
                                        bounds.Top
                                        bounds.Left
                                        (bounds.Top + effectiveHeight)
                                    ) Right (i+1)
                   
           return (_spiralLayout (Box.add 0 uiSize 0 0 boundSize) Right 0)
                   |> Some
       }
       
       
        
    let tabbedLayout uiSize boundSize container index tree : LayoutResponse =
        Box.add 0 uiSize 0 0 boundSize
        |> Some
        

    module Tests =
        open Twime.LayoutTree.Tests
        open Twime.BasicNodeReferences
        open FsUnit
        
        
        [<TestFixture; Category "engine">]
        type ``Given a one window tree`` () =
            let tree = mkTree <| C [W "One"]
            
            [<Test>]
            member x.``horizontal layout gives 100% to that window`` () =
                let hl = horizontalLayout 0 (Box.create 0 0 100 100) (byExactNode tree)
                
                hl 0 tree |> should equal <| Some (Box.create 0 0 100 100)
                
                
            [<Test>]
            member x.``vertical layout gives 100% to that window`` () =
                let vl = verticalLayout 0 (Box.create 0 0 100 100) (byExactNode tree)
                
                vl 0 tree |> should equal <| Some (Box.create 0 0 100 100)
                
            [<Test>]
            member x.``spiral layout gives 100% to that window``() =
                let sl = spiralLayout 0 (Box.create 0 0 100 100) (byExactNode tree)
                
                sl 0 tree |> should equal <| Some (Box.create 0 0 100 100)
                
        [<TestFixture; Category "engine">]
        type ``Given a tree with two windows and one container`` () =
            let tree = mkTree <| C [W "One"; W "Two"; C [W "Three"]]
            
            [<Test>]
            member x.``horizontal layout gives 50% to each node`` () =
                let hl = horizontalLayout 0 (Box.create 0 0 100 100) (byExactNode tree)
                
                hl 0 tree |> should equal <| Some (Box.create 0 0 33 100)
                hl 1 tree |> should equal <| Some (Box.create 33 0 66 100)
                hl 2 tree |> should equal <| Some (Box.create 66 0 100 100)
                
            
        [<TestFixture; Category "engine">]
        type ``Given a two window tree`` () =
            let tree = mkTree <| C [W "One"; W "Two"]
            
            [<Test>]
            member x.``horizontal layout gives 50% to each window`` () =
                let hl = horizontalLayout 0 (Box.create 0 0 100 100) (byExactNode tree)
                
                hl 0 tree |> should equal <| Some (Box.create 0 0 50 100)
                hl 1 tree |> should equal <| Some (Box.create 50 0 100 100)
                
                
            [<Test>]
            member x.``vertical layout gives 50% to each window`` () =
                let vl = verticalLayout 0 (Box.create 0 0 100 100) (byExactNode tree)
                
                vl 0 tree |> should equal <| Some (Box.create 0 0 100 50 )
                vl 1 tree |> should equal <| Some (Box.create 0 50 100 100)
                
            [<Test>]
            member x.``spiral layout gives 50% to each window``() =
                let sl = spiralLayout 0 (Box.create 0 0 100 100) (byExactNode tree)
                
                sl 0 tree |> should equal <| Some (Box.create 0 0 50 100)
                sl 1 tree |> should equal <| Some (Box.create 50 0 100 100)
                
            [<Test>]
            member x.``a window that is twice as heavy gets twice the space horizontally`` () =
                let tree =
                    LayoutTree.withLayout tree {
                        return! Weights.setWeight (BasicNodeReferences.byName "One") (Weight.create 2.0 1.0)
                    } |> Option.get
                    
                let hl = horizontalLayout 0 (Box.create 0 0 100 100) (byExactNode tree)
                    
                hl 0 tree |> should equal <| Some (Box.create 0 0 66 100)
                hl 1 tree |> should equal <| Some (Box.create 66 0 100 100)
                
            [<Test>]
            member x.``a window that is twice as heavy gets twice the space vertically`` () =
                let tree =
                    LayoutTree.withLayout tree {
                        return! Weights.setWeight (BasicNodeReferences.byName "One") (Weight.create 1.0 2.0)
                    } |> Option.get
                    
                let vl = verticalLayout 0 (Box.create 0 0 100 100) (byExactNode tree)
                    
                vl 0 tree |> should equal (Some (Box.create 0 0 100 66))
                vl 1 tree |> should equal (Some (Box.create 0 66 100 100))
                
                
                
                
                
        [<TestFixture; Category "engine">]
        type ``Given a three window tree`` () =
            let tree = mkTree <| C [
               W "One" 
               W "Two" 
               W "Three" 
            ]
            
            [<Test>]
            member x.``horizontal layout gives each one third left-to-right`` () =
                let hl = horizontalLayout 0 (Box.create 0 0 100 100) (byExactNode tree)
                
                hl 0 tree |> should equal <| Some (Box.create 0 0 33 100)
                hl 1 tree |> should equal <| Some (Box.create 33 0 66 100)
                hl 2 tree |> should equal <| Some (Box.create 66 0 100 100)
                
            [<Test>]
            member x.``vertical layout gives each one third top-to-bottom`` () =
                let vl = verticalLayout 0 (Box.create 0 0 100 100) (byExactNode tree)
                
                vl 0 tree |> should equal <| Some (Box.create 0 0 100 33)
                vl 1 tree |> should equal <| Some (Box.create 0 33 100 66)
                vl 2 tree |> should equal <| Some (Box.create 0 66 100 100)
                
            [<Test>]
            member x.``spiral layout gives half to first, quarter each to other two``() =
                let sl = spiralLayout 0 (Box.create 0 0 100 100) (byExactNode tree)
                
                sl 0 tree |> should equal <| Some (Box.create 0 0 50 100)
                sl 1 tree |> should equal <| Some (Box.create 50 0 100  50)
                sl 2 tree |> should equal <| Some (Box.create 50 50 100 100)
                
        
        [<TestFixture; Category "engine">]
        type ``Given a four window tree`` () =
            let tree = mkTree <| C [
               W "One" 
               W "Two" 
               W "Three" 
               W "Four" 
            ]
            
            [<Test>]
            member x.``horizontal layout gives each one quarter left-to-right`` () =
                let hl = horizontalLayout 0 (Box.create 0 0 100 100) (byExactNode tree)
                
                hl 0 tree |> should equal <| Some (Box.create 0 0 25 100)
                hl 1 tree |> should equal <| Some (Box.create 25 0 50 100)
                hl 2 tree |> should equal <| Some (Box.create 50 0 75 100)
                hl 3 tree |> should equal <| Some (Box.create 75 0 100 100)
                
            [<Test>]
            member x.``vertical layout gives each one quarter top-to-bottom`` () =
                let vl = verticalLayout 0 (Box.create 0 0 100 100) (byExactNode tree)
                
                vl 0 tree |> should equal <| Some (Box.create 0 0 100 25)
                vl 1 tree |> should equal <| Some (Box.create 0 25 100 50)
                vl 2 tree |> should equal <| Some (Box.create 0 50 100 75)
                vl 3 tree |> should equal <| Some (Box.create 0 75 100 100)
                
            [<Test>]
            member x.``spiral layout gives half to first, quarter to second, eighth to the later two``() =
                let sl = spiralLayout 0 (Box.create 0 0 100 100) (byExactNode tree)
                
                sl 0 tree |> should equal <| Some (Box.create 0 0 50 100)
                sl 1 tree |> should equal <| Some (Box.create 50 0 100  50)
                sl 2 tree |> should equal <| Some (Box.create 75 50 100 100)
                sl 3 tree |> should equal <| Some (Box.create 50 50 75 100)
                
            [<Test>]
            member x.``spiral layout gives more space to window with more than the average weight``() =
                let newTree = withLayout tree {
                                    return! setWeight (byName "Two") (Weight.create 2.0 2.0)
                                } |> Option.get
                
                let sl = spiralLayout 0 (Box.create 0 0 100 100) (byExactNode newTree)
                
                sl 0 newTree |> should equal (Some (Box.create 0 0 40 100))
                sl 1 newTree |> should equal (Some (Box.create 40 0 100  80))
                sl 2 newTree |> should equal (Some (Box.create 64 80 100 100))
                sl 3 newTree |> should equal (Some (Box.create 40 80 64 100))
                
                
