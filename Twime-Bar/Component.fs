namespace Twime.Bar

open System.Windows.Controls
open Twime
open Twime.LayoutUIComponents
open Views.Bar

module Styling =
    type Alignment =
        | Left
        | Inline
        | Right
        
    type T =
        {
            foreground: string option
            background: string option
            alignment: Alignment option
            padding: Box.T option
        }
        
    let defaults =
        {
            foreground = None
            background = None
            alignment = None
            padding = None
        }
        
    let withForeground fore t =
        {t with foreground = Some fore}
        
    let foreground t = t.foreground
        
    let withBackground back t =
        {t with background = Some back}
        
    let background t = t.background
    
    let withAlignment align t =
        {t with alignment = Some align}
        
    let withPadding padding t =
        {t with padding = Some padding}
        
    let padding t = t.padding

module Component =
        
    type T = {
        styling: Styling.T
        contents: TwimeRoot.T -> (TwimeRoot.Update -> unit) -> UIComponent.T -> TwimeBarComponent
    }
    
    let noUpdater _ = ()
    
    let contents c uic =
        (c.contents) uic 
        
    let styling c = c.styling
    
    let makeComponent styling f = {
        contents = f
        styling = styling
    }

