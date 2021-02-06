namespace Twime.LayoutPostProcessors

open System
open Twime

module ResizePostProcessor =
    let offset left top right bottom box =
        Box.create
            (Box.left box + left)
            (Box.top box + top)
            (Box.right box - right)
            (Box.bottom box - bottom)
            
    let postprocess a _ _ seed _ box =
        let rand = Random(seed + "resize".GetHashCode())
        let leftOffset = rand.Next(0, a)
        let topOffset = rand.Next(0, a)
        let rightOffset = rand.Next(0, a)
        let bottomOffset = rand.Next(0, a)
        
        offset leftOffset topOffset rightOffset bottomOffset box
        


