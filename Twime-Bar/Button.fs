namespace Twime.Bar

open System
open System.Windows.Controls
open Twime
open Twime.LayoutUIComponents
open Views.Bar

module Button =
    let makeButton text (runner: TwimeRoot.Update -> unit) f =
        Button(text, fun () -> runner f)
        
    let barComponent styling text f =
        Component.makeComponent styling (fun _ runner uic -> makeButton text runner f :> TwimeBarComponent)
        


