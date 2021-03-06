namespace IPC

open Twime.DirectionalNavigation
open Twime.TreeOperations

module CommandBindings =
    open CommandBinding
    open Twime
    open TreeMoveOperation
    open TreeSplitOperation
    open TreeFocusChangeOperation
    open TreeChangeWindowSettingOperation
    open TreeChangeTagSettingOperation
    open TreeSelectedOperation
    open TreeTagOperation
    open TreeDirectionalOperation

    let err : TwimeRoot.Update = fun _ -> None
    
    let parse<'a> str =
        Newtonsoft.Json.JsonConvert.DeserializeObject<'a> str
        
    type ListWrapper<'a> = {
            lst: 'a list
    }
    
    let bindDirectionTo handler args =
        match args |> msa |> normalise with
        | "up" -> handler TreeMove.Direction.Up
        | "down" -> handler TreeMove.Direction.Down
        | _ -> err
        
    let bindFourWayDirectionTo handler args =
        match args |> msa |> normalise with
        | "up" -> handler Direction.Up
        | "down" -> handler Direction.Down
        | "left" -> handler Direction.Left
        | "right" -> handler Direction.Right
        | _ -> err
        
    let bindStringTo handler args =
        handler (args |> msa)
        
    let bindStringListTo handler args =
        handler args
        
    let bindRefTo handler args =
        let _bindRef ref =
            let refNum = int64 ref |> TreeReference.fromLiteral
            (BasicNodeReferences.byRef refNum)
        handler (args |> msa |> _bindRef)
        
    let bindWeightTo handler args =
        let parseWeight floats =
            match floats with
            | [h; v] -> Weight.create (float h) (float v)
            | _ -> Weight.zero
            
        handler (args |> parseWeight)
        
    let bindBoxTo handler args =
        let parseBox ints =
            match ints with
            | [l; t; r; b] -> Box.create (int l) (int t) (int r) (int b)
            | _ -> Box.zero
            
        handler (args |> parseBox)
        
    let bindRenameOpTo handler args =
        let join (strs: string list) =
            System.String.Join(' ', strs)
            
        match args with
        | tag :: rest ->
                handler tag (join rest)
        | _ ->
            (fun _ -> None)
            
    let bindPushTagOpTo handler args =
        match args with
        | [treeRef; tagName] ->
                handler (int64 treeRef |> TreeReference.fromLiteral |> BasicNodeReferences.byRef) tagName
        | _ ->
            (fun _ -> None)
            
    let bindSetMarkOpTo handler args =
        match args with
        | [treeRef; mark] ->
                handler (int64 treeRef |> TreeReference.fromLiteral |> BasicNodeReferences.byRef) mark
        | _ ->
            (fun _ -> None)
        
        
        
    let quit _ =
        raise EventRunner.TwimeExit
        None


    let bindings = [
        (nameof moveActiveWindow) => bindDirectionTo moveActiveWindow
        (nameof swapActiveWindow) => bindDirectionTo swapActiveWindow
        (nameof splitActiveWindow) => bindStringTo splitActiveWindow
        (nameof splitActiveWindowRotate) => bindStringListTo splitActiveWindowRotate
        (nameof setActiveContainerLayoutEngine) => bindStringTo setActiveContainerLayoutEngine
        (nameof rotateActiveContainerLayoutEngine) => bindStringListTo rotateActiveContainerLayoutEngine
        (nameof splitSelection) => bindStringTo splitSelection
        (nameof splitSelectionRotate) => bindStringListTo splitSelectionRotate
        (nameof setSelectionLayoutEngine) => bindStringTo setSelectionLayoutEngine
        (nameof rotateSelectionLayoutEngine) => bindStringListTo rotateSelectionLayoutEngine
        (nameof moveWindow) => bindFourWayDirectionTo moveWindow
        (nameof swapWindow) => bindFourWayDirectionTo swapWindow
        (nameof moveFocus) => bindFourWayDirectionTo moveFocus
        (nameof changeSelectionWeight) => bindWeightTo changeSelectionWeight
        (nameof toggleGaps) => simple toggleGaps
        (nameof adjustMaxGap) => bindBoxTo adjustMaxGap
        (nameof adjustOuterGap) => bindBoxTo adjustOuterGap
        
        (nameof toggleFloating) => simple toggleFloating
        (nameof toggleFullScreen) => simple toggleFullScreen
        (nameof toggleZen) => simple toggleZen
        (nameof toggleMinimised) => simple toggleMinimised
        
        (nameof expandSelection) => simple expandSelection
        (nameof reduceSelection) => simple reduceSelection
        (nameof resetSelection) => simple resetSelection
        
        (nameof setActiveTag) => bindStringTo setActiveTag
        (nameof moveSelectionToTag) => bindStringTo moveSelectionToTag
        
        (nameof focusMark) => bindStringTo focusMark
        (nameof setMarkOnActiveWindow) => bindStringTo setMarkOnActiveWindow
        (nameof setMark) => bindSetMarkOpTo setMark
        
        (nameof quit) => simple quit
        
        "focus" => bindRefTo (TreeOperation.switchFocus BasicNodeReferences.byActiveWindow)
        "pull" => bindRefTo (TreeApiOperation.moveNode BasicNodeReferences.byLastActiveWindow)
        "renameTag" => bindRenameOpTo TreeApiOperation.renameTag
        "tag-create-action" => bindStringTo setActiveTag
        "pushTag" => bindPushTagOpTo moveSomethingToTag
    ]
    
    let parseHotkeys (configured: ConfigurationBindings.T list) =
        configured
        |> List.map (fun c ->
                    match ConfigurationBindings.action c with
                    | Integration.HotkeyAction.TreeUpdate action ->
                        (ConfigurationBindings.name c) => simple action
                    | _ ->
                        create (ConfigurationBindings.name c) Nop
            )

    let bindingFor configured name =
        (parseHotkeys configured)
        |> List.append bindings
        |> List.tryFind (isCommand name)

    module Tests =
        open Twime.LayoutTree.Tests
        open Twime.BeSameTreeAs
        open NUnit.Framework
        open FsUnit
        
        [<TestFixture>]
        type ``Given a tree with some windows`` () =
            let root =
                let activeTag = C [A "Firefox"; W "Something Else"]
                                |> mkTree
                                |> Tag.create (TreeReference.create ()) (Tag.createMeta "1" "1" None None GapConfig.none)
                
                TwimeRoot.create [
                    Display.create
                        (TreeReference.create ())
                        (Display.createMeta "disp" Box.zero Box.zero true true)
                        [activeTag]
                        (activeTag.Reference) 
                ] []

            [<Test>]
            member x.``When I move the active window down, it moves down`` () =
                bindingFor [] (nameof moveActiveWindow)
                |> Option.map (fun b -> exec b ["Down"])
                |> Option.bind (fun u -> u root)
                |> Option.get
                |> TwimeRoot.display (fun _ -> true)
                |> Display.activeLayout
                |> should beSameTreeAs (mkTree (C [W "Something Else"; A "Firefox"]))
                
                
                
            
