namespace Twime

open NUnit.Framework
open Twime.TreeNavigation
open Twime.TreeManipulation
open Twime.BasicNodeReferences

module TreeOperation =
    type FirstOptionOf<'a>(root: TwimeRoot.T, parameteriser: ('a -> TwimeRoot.Update) -> TwimeRoot.Update) =
        member this.Bind(x: 'a -> TwimeRoot.Update, f) =
            match ((parameteriser x) root) with
            | Some updater -> updater
            | None -> f None 
            
            
        member this.Combine(a, f) =
            match a with
            | Some value -> Some value
            | None -> f ()
            
        member this.Delay(f) = f
        member this.Run(f) = f()
        member this.Zero() = None
        member this.Yield(x) = (parameteriser x) root
            
    let firstOptionOf root param = FirstOptionOf(root, param)
            
    let withParameter p = (|>) p
    let noParameter r: 'b = r ()
    
    let hasWindow ref =
        (Display.anyTagHas
             (Tag.layoutHas
                  (TreeNavigation.exists ref)))
        
    let hasActiveWindow =
        hasWindow BasicNodeReferences.byActiveWindow
        
    let hasLastActiveWindow =
        hasWindow BasicNodeReferences.byLastActiveWindow
        
    let hasSelection =
        hasWindow BasicNodeReferences.bySelection
        
    let activeDisplay =
        TwimeRoot.display
            hasActiveWindow
    
    let moveNodeBetweenLayouts (sourceNode: NodeReference) (destination: NodeReference) (position: AddPosition) root =
        let _removeOldNode node layout =
            let parentLayout =
                root
                |> TwimeRoot.display (Display.anyTagHas (Tag.layoutHas (exists node)))
                |> Display.activeLayout
                
            if node parentLayout then
                let contInfo =
                    LayoutTree.containerDefinition parentLayout
                    |> Option.defaultValue (Container.create "horizontal")
                TreeManipulation.replaceNode node (fun _ -> LayoutTree.container contInfo []) layout
            else
                TreeManipulation.removeNode node layout
            
        maybe {
            let! window =
                root
                |> TwimeRoot.display (Display.anyTagHas (Tag.layoutHas (exists sourceNode)))
                |> Display.tag (Tag.layoutHas (exists sourceNode))
                |> Tag.layout
                |> find sourceNode
                    
            let! newTree = Tree.mapDisplay
                            (Display.anyTagHas (Tag.layoutHas (exists sourceNode)))
                            (Display.mapActiveLayout
                                 (_removeOldNode sourceNode))
                            root
                            
            return! Tree.mapDisplay
                        (Display.anyTagHas (Tag.layoutHas (exists destination)))
                        (Display.mapLayout
                            (fun _ t -> exists destination (Tag.layout t))
                            (TreeManipulation.addChild position destination window))
                        newTree
                        
        }
        
    let updateTree (transformations: (Window.Definition.T -> TwimeRoot.Update) list) (w: Window.Definition.T) (tree: TwimeRoot.T) =
        let mutable t = Some tree
        
        for trans in transformations do 
            t <- t |> Option.map (trans w) |> Option.flatten
        
        t
        
    let onActiveLayout f root =
        Tree.mapDisplay
            hasActiveWindow
            (Display.mapActiveLayout f)
            root
    
    let switchFocus old nu root =
        maybe {
            let deactivatedTree = Tree.mapDisplay
                                        (hasWindow old)
                                        (Display.mapLayout
                                            (fun d t -> exists old (Tag.layout t))
                                            (TreeManipulation.modifyWindow old (Window.withDefinition (Window.Definition.withActive false))))
                                        root
            let! reactivatedTree = Tree.mapDisplay
                                        (hasWindow nu)
                                        (Display.mapActiveLayout
                                            (TreeManipulation.modifyWindow nu (Window.withDefinition (Window.Definition.withActive true))))
                                        (Option.defaultValue root deactivatedTree)
                                        
            return reactivatedTree
        }

    module Tests =
        open FsUnit
        
        [<TestFixture>]
        type ``Given several operations running on a root`` () =
            let ops = [
                fun () root -> Some root
                fun () root -> None
                fun () root -> None
            ]
            let root = TwimeRoot.create [] []
            let onRoot = firstOptionOf root noParameter
            
            let testRoot id =
                TwimeRoot.create
                    [
                        Display.create
                            (TreeReference.fromLiteral id)
                            (Display.createMeta "" Box.zero Box.zero false false)
                            []
                            TreeReference.zero
                    ] []
            
            [<Test; Category "TreeOperation">]
            member x.``when the first returns Some, it is the value returned`` () =
                onRoot {
                    fun () r -> Some r
                }
                |> should equal (Some root)
                
            [<Test; Category "TreeOperation">]
            member x.``when only the second returns Some, it is the value returned`` () =
                onRoot {
                    fun () _ -> None
                    fun () _ -> Some (testRoot 1L)
                }
                |> should equal (Some (testRoot 1L))
                
            [<Test; Category "TreeOperation">]
            member x.``when the second and onward returns Some, it is the value returned`` () =
                onRoot {
                    fun () _ -> None
                    fun () _ -> Some (testRoot 1L)
                    fun () _ -> Some (testRoot 2L)
                    fun () _ -> Some (testRoot 3L)
                }
                |> should equal (Some (testRoot 1L))
                
            [<Test; Category "TreeOperation">]
            member x.``after something returns Some, further options are not evaluated`` () =
                let mutable theBadThingHappened = false
                onRoot {
                    printfn "begin!"
                    fun () _ -> None
                    printfn "one!"
                    fun () _ -> Some (testRoot 1L)
                    printfn "two!"
                    fun () _ ->
                        theBadThingHappened <- true
                        Some (testRoot 2L)
                    printfn "three!"
                    fun () _ ->
                        theBadThingHappened <- true
                        Some (testRoot 3L)
                }
                |> should equal (Some (testRoot 1L))
                
                theBadThingHappened |> should be False
