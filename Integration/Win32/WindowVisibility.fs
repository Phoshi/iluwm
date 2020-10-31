namespace Integration.Win32

open System.Runtime.InteropServices
open System
open Twime

module WindowVisibility =
    [<AutoOpen>]
    module private Native =
        [<DllImport "USER32.DLL">]
        extern bool IsWindowVisible(IntPtr hWnd)
        [<DllImport "USER32.DLL">]
        extern bool IsWindow(IntPtr hWnd)
    let public isWindowVisible handle =
        IsWindowVisible(WindowHandle.ptr handle)
    let public isWindow handle =
        IsWindow(WindowHandle.ptr handle)
