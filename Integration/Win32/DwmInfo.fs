namespace Integration.Win32

open System
open System.Runtime.InteropServices
open Twime

module DwmInfo =
    module private Native =
        type Rect =
            struct
                val left: int
                val top: int
                val right: int
                val bottom: int
            end
            
        [<DllImport "Dwmapi.dll">]
        extern IntPtr DwmGetWindowAttribute(IntPtr hWnd, int64 dwAttribute, Rect& pvAttribute, int64 cbAttribute)
        
    let extendedWindowBorders handle =
        let mutable rect = Native.Rect()
        
        Native.DwmGetWindowAttribute(WindowHandle.ptr handle, 9L, &rect, (Marshal.SizeOf rect) |> int64)
        |> ignore
        
        Twime.Box.create (int rect.left) (int rect.top) (int rect.right) (int rect.bottom)

    module Tests =
        open NUnit.Framework
        
        [<TestFixture>]
        type ``Given I am running on a windows system`` ()=
            
            [<Test>]
            member x.``I have a larger extended border than real border for no friggin' reason`` () =
                let getSizes window =
                    (WindowMetadata.title window,
                     WindowSize.size window,
                     extendedWindowBorders window)
                    
                let wins = WindowList.allWindows ()
                            |> List.map getSizes
                ()
        
