namespace Twime.LayoutPostProcessors
open Twime
open Twime.LayoutUIComponents

module PostProcessor =
    type ProcessingStyle =
        | Window
        | UI
        
    type T = Display.T -> Box.T -> int -> Window.T option -> Box.T -> Box.T

