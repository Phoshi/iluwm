namespace Twime

open System
open Twime
open Twime.Layout

module DirectionalNavigation =
    open TreeNavigation
    open LayoutTree
    
    type Direction =
        | Left
        | Right
        | Up
        | Down
        
    type DirectionalResult =
        | Window of Window.T
        | Boundary 
        
    type private DirectionType =
        | Horizontal
        | Vertical
    
    let windowLayout piece =
        match piece with
        | LayoutPiece.Window (w, b) -> Some (w, b)
        | _ -> None
        
    let isPieceFor (node: LayoutTree.T) (piece: LayoutPiece) =
        match node with
        | LayoutTree.T.WindowNode (_, win) -> 
            match piece with
            | UI _ -> false
            | LayoutPiece.Window (w, _) -> w = win
        | _ -> false
    
    let targetPoint d start =
        match d with
        | Left -> (Box.left start - 1, Box.verticalMidpoint start)
        | Up -> (Box.horizontalMidpoint start, Box.top start - 1)
        | Right -> (Box.right start + 1, Box.verticalMidpoint start)
        | Down -> (Box.horizontalMidpoint start, Box.bottom start + 1)
        
    let isPositionIn point (piece: LayoutPiece) =
        match piece with
        | UI _ -> false
        | LayoutPiece.Window (_, p) -> Box.contains point p
        
    let windowAtPoint point positions =
        positions
        |> List.tryFind (isPositionIn point)
        |> Option.map windowLayout
        |> Option.flatten
        |> Option.map (fun (w, _) -> w)
        
    let windowsInDirection direction start (positions: LayoutPiece list) =
        let windowInDirection layoutPiece =
            match windowLayout layoutPiece with
            | Some (_, position) ->
                if direction = Up then
                    Box.right position >= Box.left start
                    && Box.left position <= Box.right start
                    && Box.bottom position <= Box.top start
                else if direction = Down then
                    Box.right position >= Box.left start
                    && Box.left position <= Box.right start
                    && Box.top position >= Box.bottom start
                else if direction = Right then
                    Box.top position <= Box.bottom start
                        && Box.bottom position >= Box.top start
                        && Box.left position >= Box.right start
                else 
                    Box.top position <= Box.bottom start
                    && Box.bottom position >= Box.top start
                    && Box.right position <= Box.left start
            | _ -> false
        let notSameWindow layoutPiece =
            match windowLayout layoutPiece with
            | Some (_, position) ->
                not (position = start)
            | _ -> true
        positions
        |> List.filter windowInDirection
        |> List.filter notSameWindow
        
    let closestWindow start (contenders: LayoutPiece list) tree =
        let anyContenderIsLastActive contenders lastActive =
            contenders
            |> List.map windowLayout
            |> List.map (Option.map (fun (w, _) -> w))
            |> List.collect Option.toList
            |> List.exists ((=)lastActive)
            
        let lastActiveOfParentIfAnyExists contenders lp =
            match windowLayout lp with
            | Some (win, _) ->
                withLayout tree {
                    let! node = TreeNavigation.find (BasicNodeReferences.byWindow win)
                    let! parent = TreeNavigation.parent node
                    
                    let lastActiveChild =
                        LayoutTree.containerDefinition parent
                        |> Option.map Container.lastActiveChild
                        |> Option.flatten
                    
                    if Option.isNone lastActiveChild then
                        return Some true
                    else
                        let lastActive = Option.get lastActiveChild
                        let! lastActiveNode = TreeNavigation.find (BasicNodeReferences.byRef lastActive)
                        let lastActiveWindow =
                            LayoutTree.windows lastActiveNode
                            |> List.tryHead
                            
                        if Option.isNone lastActiveWindow then
                            return Some true
                        else if anyContenderIsLastActive contenders (Option.get lastActiveWindow) then
                            return Some ((Option.get lastActiveWindow) = win)
                        else
                            return Some true
                }
                |> Option.defaultValue true
            | _ -> true
            
        let distanceFrom point layoutPiece =
            match windowLayout layoutPiece with
            | Some (_, otherPoint) ->
                let hDistance =
                    Box.horizontalMidpoint point - Box.horizontalMidpoint otherPoint
                    |> float
                let vDistance =
                    Box.verticalMidpoint point - Box.verticalMidpoint otherPoint
                    |> float
                
                Math.Sqrt (Math.Pow(hDistance, 2.0) + Math.Pow(vDistance, 2.0))
            | _ -> Double.MaxValue
            
        let filterToCloseToMinimumDistance contenders =
            let minDistance =
                contenders
                |> List.map (distanceFrom start)
                |> List.sort
                |> List.tryHead
                
            let cutoff = Option.map ((*)1.2) minDistance
            
            List.where
                (fun c -> Option.exists ((<) (distanceFrom start c)) cutoff)
                contenders
            
            
            
        let orderedEligableContenders =
            contenders
            |> List.sortBy (distanceFrom start)
            |> filterToCloseToMinimumDistance
        
        orderedEligableContenders
        |> List.where (lastActiveOfParentIfAnyExists orderedEligableContenders)
        |> List.tryHead
        |> Option.map windowLayout
        |> Option.flatten
        |> Option.map (fun (w, _) -> w)
        
    let windowInDirection referencePoint direction (positions: LayoutPiece list) tree =
        let (_, startRect) = positions |> List.find (isPieceFor referencePoint) |> windowLayout |> Option.get
        let contenders = windowsInDirection direction startRect positions
        
        closestWindow startRect contenders tree
        
    let windowInTabbedContainer referencePoint direction positions tree =
        withLayout tree {
            let! container = parent referencePoint
            let siblings = children container
            
            let positionInChildren = List.findIndex ((=)referencePoint) siblings
            
            if positionInChildren = 0 && direction = Left then
                return windowInDirection referencePoint direction positions tree
            else if (positionInChildren + 1) = (List.length siblings) && direction = Right then
                return windowInDirection referencePoint direction positions tree
            else
                let newPosition =
                    positionInChildren + (match direction with | Left -> -1 | Right -> 1)
                    
                return List.item newPosition siblings |> LayoutTree.windows |> List.head |> Some
                    
        }
        
        
    let windowTo (layout: LayoutTree.T -> LayoutPiece list option) direction start tree =
        withLayout tree {
            let! subject = find start
            let! container = parent subject
            let! windowPositions = layout
            
            let target =
                match LayoutTree.layoutEngine container with
                | "tabbed" ->
                    if direction = Left || direction = Right then
                        windowInTabbedContainer subject direction windowPositions tree
                    else
                        windowInDirection subject direction windowPositions tree
                | _ ->
                    windowInDirection subject direction windowPositions tree
                    
            return Some <| match target  with
                            | Some w -> DirectionalResult.Window w
                            | _ -> DirectionalResult.Boundary
        }
        
        
    module Tests =
        open LayoutTree.Tests
        open BasicNodeReferences
        open NUnit.Framework
        open FsUnit
        
        let win tree name =
            let extractWindow node =
                match node with
                | WindowNode (_, w) -> Some w
                | _ -> None
                
            TreeNavigation.find (byName name) tree
            |> Option.get
            |> extractWindow
            |> Option.map Window
    
        let noUi _ _ _ _ = []
        let l = layout 0 noUi (Box.create 0 0 1000 1000)
        
        [<TestFixture>]
        type ``Given a layout with two windows in one container`` () =
            let tree = mkTree (C [W "Left"; A "Right"])
            
            let win = win tree
            
            [<Test; Category "Directional">]
            member self.``Then the window to the left from the window on the right is the window on the left`` () =
                tree
                |> windowTo l Left (byName "Right")
                |> should equal (win "Left")
                
            [<Test; Category "Directional">]
            member self.``Then the window to the right from the window on the left is the window on the right`` () =
                tree
                |> windowTo l Right (byName "Left")
                |> should equal (win "Right")
                
            [<Test; Category "Directional">]
            member self.``Then the window to the left from the window on the left is the left boundary`` () =
                tree
                |> windowTo l Left (byName "Left")
                |> should equal (Some Boundary)
                
            [<Test; Category "Directional">]
            member self.``Then the window to the up from the window on the left is the top boundary`` () =
                tree
                |> windowTo l Up (byName "Left")
                |> should equal (Some Boundary)
                
        [<TestFixture>]
        type ``Given a grid layout with four windows each given one quarter`` () =
            let tree = mkTree (C
                                   [
                                    V
                                        [
                                            W "TopLeft"
                                            A "BottomLeft"
                                        ]
                                    V
                                        [
                                            W "TopRight"
                                            W "BottomRight"
                                        ]
                                        ])
            
            let win = win tree
            
            [<Test; Category "Directional">]
            member self.``Then the window to the up from the window on the bottom left is the top left`` () =
                tree
                |> windowTo l Up (byName "BottomLeft")
                |> should equal (win "TopLeft")
                
            [<Test; Category "Directional">]
            member self.``Then the window to the up from the window on the top left is the boundary`` () =
                tree
                |> windowTo l Up (byName "TopLeft")
                |> should equal (Some Boundary)
                
            [<Test; Category "Directional">]
            member self.``Then the window to the right from the window on the bottom left is the bottom right`` () =
                tree
                |> windowTo l Right (byName "BottomLeft")
                |> should equal (win "BottomRight")
                
            [<Test; Category "Directional">]
            member self.``Then the window to the right from the window on the bottom right is the boundary`` () =
                tree
                |> windowTo l Right (byName "BottomRight")
                |> should equal (Some Boundary)
                
            [<Test; Category "Directional">]
            member self.``Then the window to the left from the window on the bottom left is the boundary`` () =
                tree
                |> windowTo l Left (byName "BottomLeft")
                |> should equal (Some Boundary)
                
            [<Test; Category "Directional">]
            member self.``Then the window to the left from the window on the bottom right is the bottom left`` () =
                tree
                |> windowTo l Left (byName "BottomRight")
                |> should equal (win "BottomLeft")
            
            [<Test; Category "Directional">]
            member self.``Then the window to the up from the window on the bottom right is the top right`` () =
                tree
                |> windowTo l Up (byName "BottomRight")
                |> should equal (win "TopRight")
                
            [<Test; Category "Directional">]
            member self.``Then the window to the up from the window on the top right is the boundary`` () =
                tree
                |> windowTo l Up (byName "TopRight")
                |> should equal (Some Boundary)
