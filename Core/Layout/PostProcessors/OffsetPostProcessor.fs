namespace Twime.LayoutPostProcessors

open System
open Twime

module OffsetPostProcessor =
    let offset left top box =
        Box.create
            (Box.left box + left)
            (Box.top box + top)
            (Box.right box + left)
            (Box.bottom box + top)
            
    let postprocess a _ _ seed box =
        let rand = Random(seed + "offset".GetHashCode())
        let leftOffset = rand.Next(-a, a)
        let topOffset = rand.Next(-a, a)
        
        offset leftOffset topOffset box
        

