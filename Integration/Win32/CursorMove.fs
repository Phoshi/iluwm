namespace Integration.Win32

open System.Runtime.InteropServices

module CursorMove =

    module private Native =
        [<DllImport "User32.dll">]
        extern bool SetCursorPos(int x, int y)
        
        type Point =
            struct
                val x: int
                val y: int
            end
            
        [<DllImport "User32.dll">]
        extern bool GetCursorPos(Point& lpPoint)
        
    let setCursorPosition x y =
        Native.SetCursorPos(x, y)
        |> ignore
        
    let getCursorPosition () =
        let mutable p = Native.Point()
        
        Native.GetCursorPos(&p)
        |> ignore
        
        (p.x, p.y)
