namespace Twime.Bar

open System.Windows
open Twime
open Views.Bar

module TrayButton =
    let makeTrayPosition (p: Point) =
        sprintf "%f %f 0" p.X p.Y
        
    let makeButton text (runner: TwimeRoot.Update -> unit) f =
        TrayButton(text, fun point -> runner (f (makeTrayPosition point)))
        
        
    let barComponent styling text f =
        Component.makeComponent styling (fun _ runner uic -> makeButton text runner f :> TwimeBarComponent)
        


