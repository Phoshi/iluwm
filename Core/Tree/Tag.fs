namespace Twime

open System.IO

module Tag =
    [<StructuredFormatDisplay "(Meta {Name})">]
    type Meta = {
        Name: string
        DisplayName: string
        Icon: string option
        Wallpaper: string option
    }
    type T =
         {Reference: TreeReference.T; Meta: Meta; Layout: LayoutTree.T}
        
    let ref t = t.Reference
    let info t = t.Meta
    let layout t = t.Layout
    let name t = t.Meta.Name
    let displayName t = t.Meta.DisplayName
    let wallpaper t = t.Meta.Wallpaper
    
    let createMeta name displayname icon wallpaper =
        {
            Name = name
            DisplayName = displayname
            Icon = icon
            Wallpaper = wallpaper
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
