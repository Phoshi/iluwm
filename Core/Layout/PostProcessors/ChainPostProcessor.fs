namespace Twime.LayoutPostProcessors

module ChainPostProcessor =
    let postprocess postprocessors display workArea seed win winBox =
         List.fold
            (fun b p -> p display workArea seed win b)
            winBox
            postprocessors

