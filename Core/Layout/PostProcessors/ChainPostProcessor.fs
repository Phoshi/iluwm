namespace Twime.LayoutPostProcessors

module ChainPostProcessor =
    let postprocess postprocessors display workArea seed winBox =
         List.fold
            (fun b p -> p display workArea seed b)
            winBox
            postprocessors

