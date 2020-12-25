namespace Twime

module Container =
    [<StructuredFormatDisplay "(Container {LayoutEngine} (LastActive {LastActiveChild}))">]
    type T = {
        LayoutEngine: string
        Selected: bool
        LastActiveChild: TreeReference.T option
        Transient: bool
        Exclusive: bool
    }
    
    let engine t = t.LayoutEngine
    let selected t = t.Selected
    let lastActiveChild t = t.LastActiveChild
    let transient t = t.Transient
    let exclusive t = t.Exclusive
    
    
    let withEngine e t =
        {t with LayoutEngine = e}
        
    let withLastActive a t =
        {t with LastActiveChild = a}
        
    let withTransient a t =
        {t with Transient = a}
        
    let intransient = withTransient false
    
    let withExclusive e t =
        {t with Exclusive = e}
        
    let withSelected s t =
        {t with Selected = s}
    
    let create engine = {LayoutEngine = engine; Selected = false; LastActiveChild = None; Transient = true; Exclusive = false}

