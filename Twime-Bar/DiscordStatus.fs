namespace Twime.Bar

open Twime.Bar
open Twime.Bar
open Views.Bar

module DiscordStatus =
    let makeStatus path notify duration =
        DiscordStatus(path, notify, duration)
        
    let barComponent styling path notify duration =
        Component.makeComponent styling (fun _ _ uic -> makeStatus path notify duration :> TwimeBarComponent)
        


