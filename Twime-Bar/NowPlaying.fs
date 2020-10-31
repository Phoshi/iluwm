namespace Twime.Bar

open Twime.Bar
open Twime.Bar
open Views.Bar

module NowPlaying =
    let makeStatus () =
        NowPlaying()
        
    let barComponent styling =
        Component.makeComponent styling (fun _ _ uic -> makeStatus () :> TwimeBarComponent)
        


