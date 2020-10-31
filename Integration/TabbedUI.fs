module Integration.TabbedUI

type Model = {
    Windows: string list
}

let init wins () =
    {
        Windows = wins
    }
    
type Msg =
    | NoOp
    
let update msg m =
    match msg with
    | NoOp -> m
    
open Elmish.WPF

let bindings () =
    [
        "Windows" |> Binding.oneWay (fun m -> m.Windows)
    ]