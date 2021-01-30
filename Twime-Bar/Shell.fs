namespace Twime.Bar

open Views.Bar

module Shell =
    let makeShell command =
        Shell(command)
        
    let barComponent styling command =
        Component.makeComponent styling (fun _ _ uic -> makeShell command :> TwimeBarComponent)
        


