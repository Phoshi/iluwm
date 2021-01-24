namespace Integration.Win32

open System.Runtime.InteropServices
open System
open System.Text
open Twime

module WindowMetadata =
    [<AutoOpen>]
    module private Native =
        [<DllImport "USER32.DLL">]
        extern bool GetWindowText(IntPtr hWnd, StringBuilder builder, int size)
        
        [<DllImport "USER32.DLL">]
        extern bool GetClassName(IntPtr hWnd, StringBuilder builder, int size)
        
        
        [<DllImport "USER32.dll">]
        extern IntPtr GetWindow(IntPtr hWnd, uint32 uCmd)
        
        [<DllImport "USER32.dll">]
        extern IntPtr GetWindowThreadProcessId(IntPtr hWnd, Int32& lpdwProcessId)
        
    let private getText textFunc handle =
        let builder = StringBuilder(256)
        textFunc(WindowHandle.ptr handle, builder, 256) |> ignore
        
        builder.ToString().Trim()
            
    let title = getText GetWindowText
        
    let className = getText GetClassName
    
    let hasOwner handle =
        GetWindow(WindowHandle.ptr handle, 4u) <> IntPtr.Zero
        
    let executableName handle =
        try
            let mutable pid: Int32 = 0
            GetWindowThreadProcessId(WindowHandle.ptr handle, &pid)
            |> ignore
            
            System.Diagnostics.Process.GetProcessById(pid).ProcessName;
        with
        | _ -> ""
        

