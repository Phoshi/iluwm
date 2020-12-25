namespace Twime

open TreeNavigation
open TreeManipulation
open BasicNodeReferences
open Twime
open TreeOperation


module TreeTagOperation =
    let private hasActiveWindow = (Display.anyTagHas (Tag.layoutHas (exists byActiveWindow)))
        
    let private isNamedTag name (tag: Tag.T) =
        (Tag.info tag).Name = name
        
    let setFocusToWindowIn name tree =
        let tag =
            TwimeRoot.display
                (Display.anyTagHas (Tag.byTagName name))
                tree
            |> Display.tag (Tag.byTagName name)
            |> Tag.layout
            
        let firstIn tag =
            LayoutTree.windowNodes tag
            |> List.tryHead
            |> Option.map (LayoutTree.ref)
            
        tag
        |> LayoutTree.lastActive
        |> Option.orElse (firstIn tag)
        |> Option.bind (fun ref -> switchFocus byActiveWindow (byRef ref) tree)
        |> Option.orElse (Some tree)
        
    let destroyInactiveEmptyTags tree =
        let _destroy display =
            Display.removeTag
                (fun t -> (not (Tag.layout t |> LayoutTree.windows |> List.isEmpty)) || Display.activeTag display = t)
                display
                
        Tree.mapDisplay
            Display.isActive
            _destroy
            tree
        
    let createTagIfNotExists tag tree =
        let tagExists =
            TwimeRoot.displays tree
            |> List.exists (Display.anyTagHas (isNamedTag tag))
            
        if tagExists then
            Some tree
        else 
            let currentEngine =
                TwimeRoot.display
                    Display.isActive
                    tree
                |> Display.activeTag
                |> Tag.layout
                |> LayoutTree.layoutEngine
                
            let tagMapping =
                TwimeRoot.tags tree
                |> TagMapping.containing tag
                
            match tagMapping with
            | Some tm ->
                let display = TagMapping.display tm
                let definition = TagMapping.tags tm |> List.find (Tag.byName tag)
                let tag = Tag.create
                            (TreeReference.create ())
                            definition
                            (LayoutTree.T.ContainerNode
                                 (TreeReference.create (), (Container.create currentEngine), []))
                            
                Tree.mapDisplay
                    (Display.isNamed (Display.name display))
                    (Display.addTag tag)
                    tree
            | _ -> None 
        
    let moveActiveWindowToTag tag =
        let _moveActiveWindowToTag root = 
            let tagLayout =
                root
                |> TwimeRoot.display (Display.anyTagHas (Tag.byTagName tag))
                |> Display.tag (Tag.byTagName tag)
                |> Tag.layout
                |> byExactNode
                
            moveNodeBetweenLayouts byActiveWindow tagLayout toEnd root
            
        Tree.apply [
            createTagIfNotExists tag
            _moveActiveWindowToTag
        ]
        
    let moveSelectionToTag tag =
        let _moveSelectionToTag root = 
            let tagLayout =
                root
                |> TwimeRoot.display (Display.anyTagHas (Tag.byTagName tag))
                |> Display.tag (Tag.byTagName tag)
                |> Tag.layout
                |> byExactNode
                
            moveNodeBetweenLayouts bySelection tagLayout toEnd root
            
        Tree.apply [
            createTagIfNotExists tag
            _moveSelectionToTag
        ]
            
    let setActiveTag tag =
        Tree.apply
            [
                createTagIfNotExists tag
                
                Tree.mapDisplay
                    (Display.anyTagHas (isNamedTag tag))
                    (Display.setActiveTag (isNamedTag tag))
                    
                destroyInactiveEmptyTags
                setFocusToWindowIn tag
            ]
            
    let addToTag tag (w: Window.Definition.T) =
        let _addWindowToTag root =
            Tree.mapTag
                (Tag.byTagName tag)
                (Tag.map (addChild toEnd byRoot (LayoutTree.window w)))
                root
                
        Tree.apply
            [
                createTagIfNotExists tag
                _addWindowToTag
            ]
                            
    let switchToTagIfWindowAlreadyExists (window: Window.Definition.T) root =
        if Tree.hasLayout (exists (byHandle window.handle)) root then
            root
            |> TwimeRoot.display
                (Display.anyTagHas (Tag.layoutHas (exists (byHandle window.handle))))
            |> Display.tag
                (Tag.layoutHas (exists (byHandle window.handle)))
            |> Tag.name
            |> fun tagName -> setActiveTag tagName root
        else
            None
        
    module Tests =
        open Twime.LayoutTree.Tests
        open FsUnit
        open NUnit.Framework
        
        [<TestFixture>]
        type ``Given a tree with two tags and one active window`` () =
            let root =
                let activeTag = C [A "Firefox"]
                                |> mkTree
                                |> Tag.create (TreeReference.create ()) (Tag.createMeta "1" "1" None None GapConfig.none)
                let passiveTag = C [] |> mkTree |> Tag.create (TreeReference.create ()) (Tag.createMeta "2" "2" None None GapConfig.none)
                
                TwimeRoot.create [
                    Display.create
                        (TreeReference.create ())
                        (Display.createMeta "disp" Box.zero Box.zero true true)
                        [activeTag; passiveTag]
                        (activeTag.Reference) 
                ] []
                
            [<Test>]
            member x.``When I move the active window to tag 2, it goes there`` () =
                let movedTree = root
                                |> moveActiveWindowToTag "2"
                                |> Option.get
                                |> TwimeRoot.display Display.isPrimary
                                
                movedTree
                |> Display.activeTagHas (Tag.layoutHas (TreeNavigation.exists byActiveWindow))
                |> should be False
                
                movedTree
                |> Display.tag (Tag.byTagName "2")
                |> Tag.layoutHas (TreeNavigation.exists byActiveWindow)
                |> should be True
                
            [<Test>]
            member x.``When I switch the active tag to 2, it is set`` () =
                let switchedTree = root
                                    |> setActiveTag "2"
                                    |> Option.get
                                    |> TwimeRoot.display Display.isPrimary
                                    
                switchedTree
                |> Display.activeTag
                |> Tag.name
                |> should equal "2"
