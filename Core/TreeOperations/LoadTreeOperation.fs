namespace Twime.TreeOperations

open Logume
open Twime

module LoadTreeOperation =
    let displays treeToLoad currentTree =
        let matchingDisplay d =
            let isDisplay =
                d
                |> Display.info
                |> Display.name
                |> Display.byName
                
            treeToLoad
            |> TwimeRoot.display isDisplay
            
        let mergeDisplay d =
            let od = matchingDisplay d
            Display.create
                (TreeReference.create ())
                (Display.info d)
                (Display.tags od)
                (Display.activeTagRef od)
            
        currentTree
        |> TwimeRoot.displays
        |> List.map mergeDisplay
        
    let appendMissingWindowsToActiveTag (oldTree: TwimeRoot.T) (newTree: TwimeRoot.T) =
        let oldWindows = Tree.windows oldTree
        let newWindows = Tree.windows newTree
        
        let missingWindows =
            oldWindows
            |> List.filter (fun w -> List.contains w newWindows)
            
        Tree.apply
            (missingWindows |> List.map Window.definition |> List.map (TreeAddOperation.addAfterActiveWindow (Logger.squash)))
            newTree
        
    let load (treeToLoad: TwimeRoot.T) currentTree =
        let newTree =
            TwimeRoot.create
                (displays treeToLoad currentTree)
                (TwimeRoot.tags currentTree)
                
        appendMissingWindowsToActiveTag currentTree newTree