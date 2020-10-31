namespace Twime.LayoutPostProcessors

module ChainPostProcessor =
    let postprocess postprocessors workArea seed winBox =
         List.fold
            (fun b p -> p workArea seed b)
            winBox
            postprocessors

