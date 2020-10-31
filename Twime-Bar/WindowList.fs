namespace Twime.Bar

open System.Windows
open System.Windows.Controls
open Twime
open Twime.LayoutUIComponents
open Views.Bar

module WindowList =
    type DC = {
        Children: Child list
    }
    and Child = {
        Name: string
        Active: bool
        Handle: TreeReference.T
    }
    let setContext children (ui: WindowList) =
        let toChild ch =
            {
                Name = UIComponent.tabName ch
                Active = UIComponent.tabActive ch
                Handle = LayoutTree.ref ch
            }
            
        let dc = {
            Children = List.map toChild children
        }
        
        ui.DataContext <- dc
        
    let windows uic =
        match (UIComponent.config uic) with
        | UIComponent.Tabs tc -> tc.directChildren
        
    let makeWindowList run f (uic: UIComponent.T) =
        let wl = WindowList(fun h -> run (f (h :?> TreeReference.T)))
        
        setContext (windows uic) wl
        
        wl
        
    let barComponent styling (f: TreeReference.T -> TwimeRoot.Update) =
        Component.makeComponent styling (fun _ run uic -> makeWindowList run f uic :> TwimeBarComponent)
        
        

