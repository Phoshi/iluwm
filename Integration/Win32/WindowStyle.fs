namespace Integration.Win32

open System.Runtime.InteropServices
open System
open Twime

module WindowStyle =
    [<AutoOpen>]
    module private Native =
        [<DllImport "USER32.DLL">]
        extern IntPtr GetWindowLongPtr(IntPtr hWnd, int index)
        
    let windowStyle handle =
        GetWindowLongPtr(WindowHandle.ptr handle, -16)
    
    let windowExStyle handle =
        GetWindowLongPtr(WindowHandle.ptr handle, -20)
    let private classHas (windowClass: IntPtr) (style: int64) =
       (windowClass.ToInt64() &&& style) = style
       
    type RegularStyle =
        | WS_VISIBLE = 0x10000000L
        | WS_SIZEBOX = 0x00040000L
        | WS_MAXIMISEBOX = 0x00010000L
        
    type ExtendedStyle =
        | WS_EX_APPWINDOW = 0x00040000L
        | WS_EX_TOOLWINDOW = 0x00000080L
        | WS_EX_NOREDIRECTIONBITMAP = 0x00200000L
        | WS_EX_NOACTIVATE = 0x08000000L
        
    type Style =
        | Style of RegularStyle
        | ExtendedStyle of ExtendedStyle
        
    let WS_VISIBLE = Style RegularStyle.WS_VISIBLE
    let WS_SIZEBOX = Style RegularStyle.WS_SIZEBOX
    let WS_MAXIMISEBOX = Style RegularStyle.WS_MAXIMISEBOX
    let WS_EX_APPWINDOW = ExtendedStyle ExtendedStyle.WS_EX_APPWINDOW
    let WS_EX_TOOLWINDOW = ExtendedStyle ExtendedStyle.WS_EX_TOOLWINDOW
    let WS_EX_NOREDIRECTIONBITMAP = ExtendedStyle ExtendedStyle.WS_EX_NOREDIRECTIONBITMAP
    let WS_EX_NOACTIVATE = ExtendedStyle ExtendedStyle.WS_EX_NOACTIVATE
    
    let has style handle =
        match style with
        | Style regularStyle ->
            let style = windowStyle handle
            classHas style (regularStyle |> int64)
        | ExtendedStyle extendedStyle ->
            let style = windowExStyle handle
            classHas style (extendedStyle |> int64)

