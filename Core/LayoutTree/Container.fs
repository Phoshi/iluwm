namespace Twime

module Container =
    [<StructuredFormatDisplay "(Container {LayoutEngine} (LastActive {LastActiveChild}))">]
    type T = {LayoutEngine: string; LastActiveChild: TreeReference.T option}
    
    let engine t = t.LayoutEngine
    let lastActiveChild t = t.LastActiveChild
    
    let withEngine e t =
        {t with LayoutEngine = e}
        
    let withLastActive a t =
        {t with LastActiveChild = a}
    
    let create engine = {LayoutEngine = engine; LastActiveChild = None}

