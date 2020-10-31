namespace Twime

open System
open Twime
open Twime.Window.WindowHandle

module WindowHandle =
    let create ptr = WindowHandle ptr
    let none = create IntPtr.Zero
    let fromWindow (w: Window.Definition.T) =
        (Window.Definition.handle w)
        
        
    let ptr handle = match handle with WindowHandle ptr -> ptr
