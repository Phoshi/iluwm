namespace Twime.Bar

open System
open System.Windows.Controls
open Twime
open Twime.LayoutUIComponents
open Views.Bar

module Spacer =
    let makeSpacer () =
        Button("", fun () -> ())
        
    let barComponent styling =
        Component.makeComponent styling (fun _ _ _ -> makeSpacer () :> TwimeBarComponent)
        


