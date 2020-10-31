namespace Integration.Rules

open Integration.Win32
open Twime

module WindowsMustExist =
    let windowMissing (w: Window.T) =
        WindowVisibility.isWindow (WindowHandle.fromWindow w.Definition)
        |> not
        
    let rule (root: TwimeRoot.T) =
        let allWindows =
            Tree.windows root
            
        let missingWindows =
            allWindows
            |> List.where windowMissing
            
        Tree.apply
            (missingWindows
             |> List.map (fun w -> 
                    (TreeRemoveOperation.removeWindow (w.Definition)))
            )
            root
            
