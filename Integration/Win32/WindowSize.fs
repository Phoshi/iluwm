namespace Integration.Win32

open System.Runtime.InteropServices
open System
open Twime

module WindowSize =
    [<AutoOpen>]
    module private Native =
        [<Struct; StructLayout(LayoutKind.Sequential)>]
        type Rect = struct
            val left: int32
            val top: int32
            val right: int32
            val bottom: int32
        end
        
        [<DllImport "USER32.dll">]
        extern bool GetWindowRect(IntPtr hWnd, Rect& rect)
        
        let WM_GETMINMAXINFO = 0x0024u
        type POINT =
            struct
                val x: uint32
                val y: uint32
            end
        
        type MINMAXINFO =
          struct
              val ptReserved: POINT
              val ptMaxSize: POINT
              val ptMaxPosition: POINT
              val ptMinTrackSize: POINT
              val ptMaxTrackSize: POINT
          end
        
        [<DllImport "USER32.dll">]
        extern IntPtr SendMessage(IntPtr hWnd, uint32 Msg, IntPtr wParam, MINMAXINFO& lParam)
        
    let size handle =
        let mutable rect = Rect()
        GetWindowRect((WindowHandle.ptr handle), &rect) |> ignore
        
        Box.create (rect.left) (rect.top) (rect.right) (rect.bottom)
        
    let minimumSize handle =
        let mutable minmaxinfo = MINMAXINFO()
        SendMessage((WindowHandle.ptr handle), WM_GETMINMAXINFO, IntPtr.Zero, &minmaxinfo)
        |> ignore
        
        let minSize = minmaxinfo.ptMinTrackSize
        
        Box.create 0 0 (int32 minSize.x) (int32 minSize.y)

