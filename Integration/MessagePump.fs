namespace Integration

open System
open System.Runtime.InteropServices

module MessagePump =
    [<AutoOpen>]
    module private Native =
        type Point =
            struct
                val x: int64
                val y: int64
            end
            
        type WinMsg =
            struct
                val hwnd: IntPtr
                val message: uint32
                val wParam: IntPtr
                val lParam: IntPtr
                val time: uint32
                val pt: Point
            end
            
        [<DllImport "user32.dll">]
        extern int GetMessage(WinMsg& lpmessage, IntPtr hWnd, uint32 wMsgFilterMin, uint32 wMsgFilterMax)
        
        [<DllImport "user32.dll">]
        extern bool TranslateMessage(WinMsg& lpMsg)
        
        [<DllImport "user32.dll">]
        extern IntPtr DispatchMessage(WinMsg& lpMsg)

    let pumpQueue () =
       let mutable msg: WinMsg = WinMsg()
       
       while GetMessage(&msg, IntPtr.Zero, 0u, 0u) > 0 do 
           TranslateMessage(&msg) |> ignore
           DispatchMessage(&msg) |> ignore
