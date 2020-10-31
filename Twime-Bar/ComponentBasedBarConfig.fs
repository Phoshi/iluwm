namespace Twime.Bar

open System.Globalization
open System.Windows
open Twime
open Twime.Bar
open Twime.LayoutUIComponents
open Views.Bar

module ComponentBasedBarConfig =
    let getComponentsFor primary secondary uic =
        if UIComponent.isRoot uic then
            primary
        else secondary
        
    let create primaryComponents components root runner uic =
        Bar.bar
            (UIComponent.box uic)
            root
            runner
            (getComponentsFor primaryComponents components uic)
            uic
        :> Window
            
    let update primaryComponents components (w: Window) root runner uic =
        match w with
        | :? Bar as bar ->
            Bar.setItems
                root 
                runner
                (getComponentsFor primaryComponents components uic)
                uic bar
            
    let destroy (w: Window) =
        w.Close()
        
    let config primaryComponents components =
        UiConfig.make
            (create primaryComponents components)
            (update primaryComponents components)
            destroy
        
    

