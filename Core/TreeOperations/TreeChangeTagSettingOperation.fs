namespace Twime

module TreeChangeTagSettingOperation =
    let withActiveWindow = 
        (Tag.layoutHas (TreeNavigation.exists BasicNodeReferences.byLastActiveWindow))
    
    let toggleGaps =
        let _toggleGaps tag =
            tag
            |> Tag.withGapConfig (tag.Meta.GapConfig |> GapConfig.withEnabled (not tag.Meta.GapConfig.enable))
            |> Some
        Tree.mapTag
            withActiveWindow
            _toggleGaps
            
    let adjustMaxGap adjust =
        let _toggleGaps tag =
            tag
            |> Tag.withGapConfig (tag.Meta.GapConfig |> GapConfig.withMax (tag.Meta.GapConfig.max |> Option.map (Box.addBox adjust)))
            |> Some
        Tree.mapTag
            withActiveWindow
            _toggleGaps
            
    let adjustOuterGap adjust =
        let _toggleGaps tag =
            tag
            |> Tag.withGapConfig (tag.Meta.GapConfig |> GapConfig.withOuter (tag.Meta.GapConfig.outer |> Option.map (Box.addBox adjust)))
            |> Some
        Tree.mapTag
            withActiveWindow
            _toggleGaps 

