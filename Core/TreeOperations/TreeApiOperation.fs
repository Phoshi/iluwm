namespace Twime
open TreeNavigation
open Twime
open BasicNodeReferences
open TreeManipulation

module TreeApiOperation =
    let moveNode target node root =
        let nodeDef =
            Tree.windowNodes root
            |> List.tryFind node
            |> Option.bind (LayoutTree.windowDefinition)
            
        match nodeDef with
        | Some nd ->
            let adder r = 
                (TreeOperation.firstOptionOf r (TreeOperation.withParameter nd.Definition) {
                    TreeAddOperation.addAfter target
                    TreeAddOperation.addToRootOfClosestDisplay
                    TreeAddOperation.addToRootOfPrimaryDisplay
                })
                
            Tree.apply
                [
                    TreeRemoveOperation.removeWindow nd.Definition
                    adder
                    TreeOperation.switchFocus byActiveWindow (byHandle nd.Definition.handle)
                ]
                root
        | None -> None

    let renameTag tag name root =
        Tree.mapTag
            (Tag.byTagName tag)
            (fun t -> Tag.rename name t |> Some)
            root