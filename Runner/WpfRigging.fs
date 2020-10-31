module Runner.WpfRigging

open System.Windows
open Elmish.WPF
open Integration
open Views

let beginWpf () =
    let a = Application()
    a.ShutdownMode <- ShutdownMode.OnExplicitShutdown
    
    a
    
let runWpf (a: Application) =
    a.Run()
    
let mkWindow () =
    Program.mkSimpleWpf (TabbedUI.init []) TabbedUI.update TabbedUI.bindings
    |> Program.runWindow (ContainerWindow())

