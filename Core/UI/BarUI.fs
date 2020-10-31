namespace Twime.UI
open Twime
open TreeNavigation
open Twime.LayoutUIComponents

module BarUI =
    let ui (display: Display.T) uiSize boundSize container tree =
        let tabbedUi box =
            let c = find container tree
            let isRoot = Option.contains tree c
            
            c
            |> Option.map children
            |> Option.map (UIComponent.tabs (Option.get c) box isRoot (Display.isPrimary display))
            |> Option.map (fun t -> [t])
            |> Option.defaultValue []
            
        tabbedUi (
                Box.create
                    (Box.left boundSize)
                    (Box.top boundSize)
                    (Box.right boundSize)
                    (Box.top boundSize + uiSize)
                 )

