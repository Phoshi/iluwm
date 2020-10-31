namespace Twime

module Display =
    [<StructuredFormatDisplay "(Meta (Name {Name}) {WorkArea} (Active {ActiveTag}))">]
    type Meta = {
        Name: string; WorkArea: Box.T; DisplayArea: Box.T; Primary: bool; Active: bool;
    }
    
    [<StructuredFormatDisplay "(Display {Reference} {Meta} {Tags}">]
    type T =
        {Reference: TreeReference.T; Meta: Meta; ActiveTag: TreeReference.T; Tags: Tag.T list}
        
    let tags t = t.Tags
    let ref t = t.Reference
    let info t = t.Meta
    
    let workArea (d: T) = d.Meta.WorkArea
    let displayArea (d: T) = d.Meta.DisplayArea
    
    let isPrimary (d: T) = d.Meta.Primary
    let isActive (d: T) = d.Meta.Active
    
    let name (d: Meta) = d.Name
    let primary (d: Meta) = d.Primary
    
    let isDisplay d1 d2 =
        (ref d1) = (ref d2)
        
    let isNamed n d =
        n = (d |> info |> name)
        
    let activeTagRef display =
        display.ActiveTag
        
    let isActiveTag display tag =
        (Tag.ref tag) = (activeTagRef display)
        
    let activeTag display =
        List.find (isActiveTag display) (tags display)
        
    let activeLayout d =
        activeTag d
        |> Tag.layout
        
    let inactiveTags display =
        List.where ((<>) (activeTag display)) (tags display)
        
    let activeTagHas f d =
        activeTag d
        |> f
        
    let activeLayoutHas f d =
        activeTag d
        |> Tag.layoutHas f
        
    let anyTagHas f d =
        tags d
        |> List.exists f
        
    let byName n d =
        (d |> info |> name) = n
        
        
    let create ref meta tags activeTag = {Reference = ref; Meta = meta; Tags = tags; ActiveTag = activeTag; }
    let createMeta name workarea displayarea primary active = {Name = name; WorkArea = workarea; DisplayArea = displayarea; Primary = primary; Active = active}
    
    
    let map f t =
        tags t
        |> List.map f
        |> Maybe.lift
        |> Option.map (fun tags -> create (ref t) (info t) tags (activeTagRef t))
        
    let mapTag pred f d =
        tags d
        |> List.map (fun t -> if pred d t then f t else Some t)
        |> Maybe.lift
        |> Option.map (fun tags -> create (ref d) (info d) tags (activeTagRef d))
        
    let mapActiveTag =
        mapTag isActiveTag
        
    let mapLayout pred f d =
        mapTag pred
            (fun t ->
                (Tag.map f t))
            d
    let mapActiveLayout =
        mapLayout isActiveTag
        
    let tag pred d =
        tags d
        |> List.find pred
        
    let setActiveTag pred (d: T)=
        let t = tag pred d
        
        Some {d with ActiveTag = t.Reference}
        
    let addTag tag d =
        Some {d with Tags = d.Tags @ [tag]}
    let removeTag tag d =
        Some {d with Tags = List.filter (fun t -> tag t) d.Tags}
        
    let setActive state (d: T) =
        Some {d with Meta = {d.Meta with Active = state}}

