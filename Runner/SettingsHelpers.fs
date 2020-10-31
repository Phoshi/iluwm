namespace Runner

open Twime.Bar

[<AutoOpen>]
module SettingsHelpers =
    let gapWidth n =
        Some (Twime.Box.create n n -n -n)
    
    let basicTag identifier =
        Twime.Tag.createMeta identifier identifier None None
        
    let namedTag identifier name =
        Twime.Tag.createMeta identifier name None None
        
    let fullTag identifier name wallpaper =
        Twime.Tag.createMeta identifier name None (Some wallpaper)
        
    let withIcon icon tag =
        Twime.Tag.metaWithIcon icon tag
        
    let andColoured back fore style =
        style
        |> Styling.withBackground back
        |> Styling.withForeground fore
        
    let coloured back fore =
        andColoured back fore Styling.defaults
        
    let andAligned align style =
        style
        |> Styling.withAlignment align
        
    let aligned align =
        andAligned align Styling.defaults
        
    let andPadded l r style =
        style
        |> Styling.withPadding (Twime.Box.create l 0 r 0)