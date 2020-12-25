namespace Twime

module GapConfig = 
    type T = {
        max: Box.T option
        min: Box.T option
        outer: Box.T option
        enable: bool
    }
    
    let min t = t.min
    let max t = t.max
    let enabled t = t.enable
    
    let withEnabled enabled t =
        {t with enable = enabled}
        
    let withMax gap t =
        {t with max = gap}
        
    let withOuter gap t =
        {t with outer = gap}
    
    let create max min outer on =
        {
         max = max
         min = min
         outer = outer
         enable = on
        }
        
    let none = create None None None true

module Tag =
    
    [<StructuredFormatDisplay "(Meta {Name})">]
    type Meta = {
        Name: string
        DisplayName: string
        Icon: string option
        Wallpaper: string option
        GapConfig: GapConfig.T
    }
    type T =
         {Reference: TreeReference.T; Meta: Meta; Layout: LayoutTree.T}
        
    let ref t = t.Reference
    let info t = t.Meta
    let layout t = t.Layout
    let name t = t.Meta.Name
    let displayName t = t.Meta.DisplayName
    let wallpaper t = t.Meta.Wallpaper
    
    let gapConfig t = t.Meta.GapConfig
    let withGapConfig conf t =
        {t with Meta = {t.Meta with GapConfig = conf}}
    
    let createMeta name displayname icon wallpaper gapConfig =
        {
            Name = name
            DisplayName = displayname
            Icon = icon
            Wallpaper = wallpaper
            GapConfig = gapConfig
        }
        
    let metaWithIcon icon meta =
        {meta with Icon = icon}
        
    let create ref tagInfo layout = {Reference = ref; Meta = tagInfo; Layout = layout}
    
    let map f t =
        layout t
        |> f
        |> Option.map (create (ref t) (info t)) 
    
    let maybeMap f t =
        match (layout t) |> f with
        | Some result -> create (ref t) (info t) result
        | None -> t
        
    let layoutHas f t =
        layout t
        |> f
        
    let byName name (tag: Meta) =
        tag.Name = name
        
    let byTagName name tag =
        info tag
        |> byName name
