namespace Twime.Bar

open System
open System.Windows.Controls
open Twime
open Twime.LayoutUIComponents
open Views.Bar

module LayoutIndicator =
    let symbolise (names: Map<string, string list>) uic engine =
        let names = names.[engine]
        let rand = 
            (UIComponent.container uic |> LayoutTree.ref).GetHashCode()
            |> Random
        List.item (rand.Next(names.Length)) names
        
    let makeIndicator names (runner: TwimeRoot.Update -> unit) f (uic: UIComponent.T) =
        let fNext () =
            uic
            |> UIComponent.container
            |> LayoutTree.ref
            |> BasicNodeReferences.byRef
            |> f
            |> runner
            
        UIComponent.container uic
        |> LayoutTree.layoutEngine
        |> symbolise names uic
        |> fun s -> Button(s, fun () -> fNext())
    let barComponent styling names f =
        Component.makeComponent styling (fun _ runner uic -> makeIndicator names runner f uic :> TwimeBarComponent)
        


