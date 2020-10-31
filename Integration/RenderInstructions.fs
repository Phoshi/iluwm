namespace Integration

open Twime.LayoutUIComponents

module RenderInstructions =
    open Twime
    
    type T = {
        visibleWindows: (Window.T * Box.T) list
        invisibleWindows: Window.T list
        activeDisplay: Display.T option
        UIs: UIComponent.T list
        tree: TwimeRoot.T
        wallpapers: (Box.T * string option) list
    }
    
    let visibleWindows t =
        t.visibleWindows
        
    let invisibleWindows t =
        t.invisibleWindows
        
    let uis t = t.UIs
    
    let wallpaper t = t.wallpapers
    
    let root t = t.tree
        
    let activeDisplay t =
        t.activeDisplay
    
    let create visible invisible display uis tree wallpapers =
        {
            visibleWindows = visible
            invisibleWindows = invisible
            activeDisplay = display
            UIs = uis
            tree = tree
            wallpapers = wallpapers
        }

