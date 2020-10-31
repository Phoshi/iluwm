namespace Twime.Bar

open System
open System.Diagnostics
open System.Management
open System.Windows.Controls
open Twime
open Twime.LayoutUIComponents
open Views.Bar

module PerformanceCounter =
    let makeCounter f =
        PerformanceMeasure(fun () -> f())
        
    let cpuCounter = new PerformanceCounter("Processor", "% Processor Time", "_Total");
    let ramCounter = new PerformanceCounter("Memory", "Available MBytes");
        
    let cpu () =
        cpuCounter.NextValue().ToString() + "%"
        
    let ram () =
        ramCounter.NextValue().ToString() + "MB"
        
    let barComponent styling f =
        Component.makeComponent styling (fun _ runner uic -> makeCounter f :> TwimeBarComponent)
        


