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
            let dir = matcher direction
            
            (dir box) = (dir workArea)
            
            
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
        
    let scale (min: Box.T) (max: Box.T) scaleFactor =
        if scaleFactor = 0.0f then max
        else
            
        let scaled factor direction =
            let difference = (direction max) - (direction min)
            
            (direction min) + (int ((float32 difference) * factor))
            
        let factor = 1.0f / scaleFactor
        
        let scaledSide = scaled factor
        
        Box.create
            (scaledSide Box.left)
            (scaledSide Box.top)
            (scaledSide Box.right)
            (scaledSide Box.bottom)
            
    
    let scalingFactor display =
        let tree =
            display
            |> Display.activeTag
            |> Tag.layout
            
        let rec _scalingFactor (tree: LayoutTree.T) =
            match tree with
            | LayoutTree.WindowNode (_, w) ->
                if w.Definition.ignoreForHigherOrderLayout then 0.f
                else if w.Definition.floating then 0.f 
                else Window.weight w |> Weight.average |> float32
            | LayoutTree.ContainerNode (_, ci, ws) ->
                if (ci.LayoutEngine = "tabbed") then
                    List.map _scalingFactor ws
                    |> List.fold (fun max factor -> if factor > max then factor else max) 0f
                else
                    List.map _scalingFactor ws
                    |> List.sum
            
        _scalingFactor tree
        
    let postprocessScaling style (minSize: Box.T) (maxSize: Box.T) display workArea seed box =
        let gapConfig =
            Display.activeTag display
            |> Tag.gapConfig
            
        if gapConfig.enable then
            let gapSize dir def =
                dir gapConfig
                |> Option.filter (fun _ -> style = PostProcessor.ProcessingStyle.Window)
                |> Option.defaultValue def
            let effectiveSize =
                scale (gapSize GapConfig.min minSize) (gapSize GapConfig.max maxSize) (scalingFactor display)
            
            addGap effectiveSize workArea box
        else
            box
        
    let postprocess style size display workArea seed box =
        postprocessScaling style size size display workArea seed box 

