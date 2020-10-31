namespace Twime.Bar

open System.Windows
open Twime
open Twime.LayoutUIComponents

module UiConfig =
    type T = {
        createUi: TwimeRoot.T -> (TwimeRoot.Update -> unit) -> UIComponent.T -> Window
        updateUi: Window -> TwimeRoot.T -> (TwimeRoot.Update -> unit) -> UIComponent.T -> unit
        destroyUi: Window -> unit
    }
    
    let create t r c = t.createUi r c 
    let update t w r c = t.updateUi w r c
    let destroy t w = t.destroyUi w
    
    let make createUi updateUi destroyUi =
        {
            createUi = createUi
            updateUi = updateUi
            destroyUi = destroyUi
        }

