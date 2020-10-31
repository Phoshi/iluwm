namespace Twime.LayoutPostProcessors

module GapsPostProcessor =
    open Twime
    type Direction = Left | Top | Right | Bottom
    let private addGap size workArea box =
        let matcher direction =
            match direction with
                | Left -> Box.left
                | Top -> Box.top
                | Right -> Box.right
                | Bottom -> Box.bottom
        let doFullGap direction =
            true
            
        let gap direction =
            if doFullGap direction then
                (matcher direction size)
            else
                (matcher direction size) / 2
                
        Box.add
            (gap Left)
            (gap Top)
            (gap Right)
            (gap Bottom)
            box
        
        
    let postprocess size workArea seed box =
        addGap size workArea box

