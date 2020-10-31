namespace Twime.Bar

open System.Windows
open System.Windows.Controls
open Twime
open Twime.LayoutUIComponents
open Views.Bar

module TagList =
    type DC = {
        Tags: Child list
    }
    and Child = {
        Name: string
        DisplayName: string
        Active: bool
        Count: int
    }
    let setContext root uic (ui: TagDisplay) =
        let toChild display ch =
            {
                Name = (Tag.name ch)
                DisplayName = (Tag.displayName ch)
                Active = (Display.activeTag display = ch)
                Count = (Tag.layout ch |> LayoutTree.windows |> List.length)
            }
            
        let activeDisplay =
            TwimeRoot.display
                (Display.activeLayoutHas (TreeNavigation.exists (UIComponent.container uic |> BasicNodeReferences.byExactNode)))
                root
                
        let tags =
            Display.tags activeDisplay
            
        let dc = {
            Tags = List.map (toChild activeDisplay) tags |> List.sortBy (fun c -> c.Name)
        }
        
        ui.DataContext <- dc
        
    let makeTagList root run f (uic: UIComponent.T) =
        let wl = TagDisplay(fun h -> run (f h))
        
        setContext root uic wl
        
        wl
        
    let barComponent styling (f: string -> TwimeRoot.Update) =
        Component.makeComponent styling (fun root run uic -> makeTagList root run f uic :> TwimeBarComponent)
        
        

