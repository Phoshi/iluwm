namespace Twime

open Twime.TreeMove
open Twime.TreeOperation


module TreeMoveOperation =
    let moveActiveWindow direction : TwimeRoot.Update =
        Tree.mapDisplay
            hasActiveWindow
            (Display.mapActiveLayout
                 (TreeMove.move direction BasicNodeReferences.byActiveWindow))
            
    let swapActiveWindow direction : TwimeRoot.Update =
        Tree.mapDisplay
            hasActiveWindow
            (Display.mapActiveLayout
                 (TreeMove.swap direction BasicNodeReferences.byActiveWindow))

    let newIndex totalNumberOfMonitors direction fromIndex =
        (fromIndex
            + (match direction with
               | Up -> -1
               | Down -> 1))
            % (totalNumberOfMonitors)
            |> fun i -> if i < 0 then (i + totalNumberOfMonitors) else i
                
    let nextDisplay direction currentDisplay root =
        root
        |> TwimeRoot.displays
        |> List.findIndex ((=)currentDisplay)
        |> newIndex (root |> TwimeRoot.displays |> List.length) direction
        |> fun i -> List.item i (TwimeRoot.displays root)
        |> Display.activeTag
        |> Tag.layout
        |> BasicNodeReferences.byExactNode
        
    let moveActiveWindowBetweenMonitors (direction: Direction) root =
        let currentDisplay =
            root
            |> TwimeRoot.display
                   (Display.anyTagHas
                        (Tag.layoutHas
                             (TreeNavigation.exists BasicNodeReferences.byActiveWindow)))
                   
        TreeOperation.moveNodeBetweenLayouts
            BasicNodeReferences.byActiveWindow
            (nextDisplay direction currentDisplay root)
            TreeManipulation.toEnd
            root
            
    module Tests =
        open NUnit.Framework
        open FsUnit
        
        [<TestFixture; Category "WindowMove">]
        type ``Given two displays`` () =
            
            [<Test>]
            member x.``The next display after the first is the last`` () =
                newIndex 2 Down 0
                |> should equal 1
                
            [<Test>]
            member x.``The next display after the last is the first`` () =
                newIndex 2 Down 1
                |> should equal 0