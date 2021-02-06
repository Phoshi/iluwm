namespace Twime

open System
open Twime
open Twime.Window

module SpecialCase =
    module Predicate =
        let entitled title (window: Window.Definition.T) =
            window.title = title
            
        let executable (processName: string) (window: Window.Definition.T) =
            window.processName.ToLowerInvariant() = processName.ToLowerInvariant()
            
    module Action =
        let transform transformer continuation (window: Window.Definition.T) (root: TwimeRoot.T): TwimeRoot.T option =
            (transformer window)
            |> continuation
            <| root
            
    module Transformer =
        let nothing _ w = w
        let title newTitle (window: Window.Definition.T) =
            {window with title = newTitle}
            
        let weight (h, f) (window: Window.Definition.T) =
            {window with weight = Weight.create h f }
            
        let mark m (window: Window.Definition.T) =
            {window with marks = [m]}
            
        let zen state (w: Window.Definition.T) =
            {w with zen = state}
            
        let maximisation state (w: Window.Definition.T) =
            {w with maximised = state}
            
        let floating state (w: Window.Definition.T) =
            {w with floating = state}
            
        let properties transformers window =
            List.fold (fun w t -> t w) window transformers
            
    type T =
        {
            predicate: Window.Definition.T -> bool
            action: Window.Definition.T -> TwimeRoot.Update
        }
        
    let predicate t = t.predicate
    let action t = t.action
        
    let create p a =
        {predicate = p; action = a}

module SpecialCaseAwareOperation =
    let inline (=>) pred action = SpecialCase.create pred action
    let handleSpecialCases (cases: SpecialCase.T list) window root =
        let specialCase =
            cases
            |> List.tryFind (fun c -> c.predicate window)
            
        specialCase
        |> Option.map SpecialCase.action
        |> Option.defaultValue (fun _ _ -> None)
        <| window
        <| root
        
    module Tests =
        open LayoutTree.Tests
        open Tree.Tests
        open FsUnit
        open NUnit.Framework
        
        let windowDef name =
            Window.Definition.create Box.zero Weight.init Box.zero name "" false false false (WindowHandle.create IntPtr.Zero)
            
        [<TestFixture>]
        type ``Given an empty tree`` () =
            let tree layout =
                TwimeRoot.create [
                    d "display" Box.zero [
                        t "taaaag" layout
                    ]
                ] []
                
            let emptyTree = tree (mkTree (C []))
            let layoutFor (tree: TwimeRoot.T option) =
                tree
                |> Option.map (fun t -> t.Displays |> List.head)
                |> Option.map (fun d -> d.Tags |> List.head)
                |> Option.map Tag.layout
                |> Option.get
            
            [<Test>]
            member x.``when I have no special cases then any window is unhandled`` () =
                handleSpecialCases [] (windowDef "Nothing special") emptyTree
                |> should equal None
                
            [<Test>]
            member x.``when I have a special case then a non-matching window is unhandled`` () =
                handleSpecialCases [SpecialCase.create (SpecialCase.Predicate.entitled "Something special") TreeAddOperation.addToRootOfPrimaryDisplay] (windowDef "Nothing special") emptyTree
                |> should equal None
                
            [<Test>]
            member x.``when I have a special case then a matching window is handled`` () =
                handleSpecialCases [SpecialCase.create (SpecialCase.Predicate.entitled "Something special") TreeAddOperation.addToRootOfPrimaryDisplay] (windowDef "Something special") emptyTree
                |> layoutFor
                |> should BeSameTreeAs.beSameTreeAs (mkTree (C [W "Something special"]))
                
            [<Test>]
            member x.``when I have a special case then a matching window is transformed`` () =
                handleSpecialCases [SpecialCase.create (SpecialCase.Predicate.entitled "Something special") (SpecialCase.Action.transform (SpecialCase.Transformer.title "Something unique") TreeAddOperation.addToRootOfPrimaryDisplay)] (windowDef "Something special") emptyTree
                |> layoutFor
                |> should BeSameTreeAs.beSameTreeAs (mkTree (C [W "Something unique"]))

module SyntaxTests =
    open SpecialCase.Predicate
    open SpecialCase.Transformer
    open SpecialCase.Action
    open SpecialCaseAwareOperation
    let rules = [
        entitled "Firefox" => transform (weight (0.5, 0.5)) TreeAddOperation.addToRootOfPrimaryDisplay
        executable "KeePass" => (TreeAddOperation.addAndSplitActiveWindow (60.0f/40.0f))
    ]
    
