namespace Runner

open Twime.Bar
open Twime

[<AutoOpen>]
module SettingsHelpers =
    let gapWidth n =
        Some (Twime.Box.create n n -n -n)
        
    let gapLetterbox n =
        Some (Twime.Box.create 0 n 0 -n)
        
    let gapRectangle h w =
        Some (Twime.Box.create w h -w -h)
    
    let basicTag identifier =
        Twime.Tag.createMeta identifier identifier None None GapConfig.none
        
    let namedTag identifier name =
        Twime.Tag.createMeta identifier name None None GapConfig.none
        
    let fullTag identifier name wallpaper =
        Twime.Tag.createMeta identifier name None (Some wallpaper) GapConfig.none
        
    let andWithGaps (config: GapConfig.T) (tag: Tag.Meta) =
        {tag with GapConfig = config}
        
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