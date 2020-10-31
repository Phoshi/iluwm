namespace Integration.TreeOperations

open System
open Integration.Win32
open Twime
open Logume

module LogOperation =
    let logWindow prefix log w _ =
        let toBin (i: IntPtr) =
            Convert.ToString(i |> int, 2)
        let windowClass = WindowMetadata.className (WindowHandle.fromWindow w)
        let windowStyle = WindowStyle.windowStyle (WindowHandle.fromWindow w)
        
        Message.message (sprintf "%s: %s[%s:%s] (%s)" prefix w.title windowClass (toBin windowStyle) (w.size |> Box.toString))
        |> Message.trivial
        |> log
        
        None
        

