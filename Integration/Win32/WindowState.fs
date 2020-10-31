namespace Integration.Win32

open System.Runtime.InteropServices
open System
open Twime

module WindowState =
    [<AutoOpen>]
    module private Native =
        [<DllImport "USER32.DLL">]
        extern IntPtr GetForegroundWindow()
        type POINT =
            struct
                val x: uint32
                val y: uint32
            end
            
        [<Struct; StructLayout(LayoutKind.Sequential)>]
        type Rect =
            struct
                val left: int32
                val top: int32
                val right: int32
                val bottom: int32
            end
            
        type WINDOWPLACEMENT =
            struct
                val mutable length: uint32
                val flags: uint32
                val showCmd: uint32
                val minPosition: POINT
                val maxPosition: POINT
                val normalPosition: Rect
                val rcDevice: Rect
            end 
        
        [<DllImport "User32.dll">]
        extern bool GetWindowPlacement(IntPtr hWnd, WINDOWPLACEMENT& wp)
        
        [<DllImport "User32.dll">]
        extern IntPtr GetWindowThreadProcessId(IntPtr hWnd, IntPtr processId)
        
        [<DllImport "User32.dll">]
        extern bool AttachThreadInput(IntPtr idAttach, IntPtr idAttachTo, bool fAttach)
        
        [<DllImport "User32.dll">]
        extern bool BringWindowToTop(IntPtr hWnd)
        
        [<DllImport "User32.dll">]
        extern bool SystemParametersInfo(uint32 uiAction, uint32 uiParam, IntPtr pvParam, uint32 fWinIni)
        
        let SPI_GETFOREGROUNDLOCKTIMEOUT = 0x2000u;
        let SPI_SETFOREGROUNDLOCKTIMEOUT = 0x2001u;
        
        [<DllImport "User32.dll">]
        extern bool SetForegroundWindow(IntPtr hWnd)
        
        [<DllImport "User32.dll">]
        extern bool IsWindowVisible(IntPtr hWnd)
        
        [<DllImport "User32.dll">]
        extern bool IsIconic(IntPtr hWnd)
        
        [<DllImport "User32.dll">]
        extern bool ShowWindowAsync(IntPtr hWnd, int nCmdShow)
        
        let SW_RESTORE = 9;
        
    let foregroundWindow () =
        GetForegroundWindow()
        |> WindowHandle.create
        
        
    let public isForeground handle =
        WindowHandle.ptr handle = GetForegroundWindow()
        
    let attemptSetForeground target currentForeground =
        SetForegroundWindow(WindowHandle.ptr target) |> ignore
        System.Threading.Thread.Sleep(10)
        
        
    let public setForeground handle =
        if (foregroundWindow () = handle) then
            ()
        else
            if (IsIconic(WindowHandle.ptr handle)) then
                ShowWindowAsync((WindowHandle.ptr handle), SW_RESTORE) |> ignore
            
            attemptSetForeground handle (foregroundWindow ())
            
            let foregroundThread = GetWindowThreadProcessId(WindowHandle.ptr (foregroundWindow ()), IntPtr.Zero)
            let thisThread = System.Threading.Thread.CurrentThread.ManagedThreadId |> IntPtr
            let targetThread = GetWindowThreadProcessId(WindowHandle.ptr handle, IntPtr.Zero)
            
            let meAttachedToFore = AttachThreadInput(thisThread, foregroundThread, true)
            let foreAttachedToTarget = AttachThreadInput(foregroundThread, targetThread, true)
            
            let foreground = foregroundWindow ()
            BringWindowToTop(WindowHandle.ptr handle) |> ignore
            
            for _ in 0..5 do
                if (foregroundWindow () <> handle) then
                    attemptSetForeground handle (foregroundWindow ())
                    
            AttachThreadInput(foregroundThread, thisThread, false) |> ignore
            AttachThreadInput(foregroundThread, targetThread, false) |> ignore
            
            if not (isForeground handle) then
                let timeout = IntPtr.Zero
                SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0u, timeout, 0u) |> ignore
                SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0u, IntPtr.Zero, 0x1u) |> ignore
                BringWindowToTop(WindowHandle.ptr handle) |> ignore
                SetForegroundWindow(WindowHandle.ptr handle) |> ignore
                SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0u, timeout, 0x1u) |> ignore
                
            if meAttachedToFore then
                AttachThreadInput(thisThread, foregroundThread, false) |> ignore
            if foreAttachedToTarget then
                AttachThreadInput(foregroundThread, targetThread, false) |> ignore
            
            
        
    let public isMaximised handle =
        let mutable placement = WINDOWPLACEMENT()
        placement.length <- uint32 <| Marshal.SizeOf placement
        
        GetWindowPlacement(WindowHandle.ptr handle, &placement)
        |> ignore
        
        placement.showCmd = (uint32 3)

        
    let public isMinimised handle =
        let mutable placement = WINDOWPLACEMENT()
        placement.length <- uint32 <| Marshal.SizeOf placement
        
        GetWindowPlacement(WindowHandle.ptr handle, &placement)
        |> ignore
        
        placement.showCmd = (uint32 2)
        
    let public isVisible handle =
        IsWindowVisible(WindowHandle.ptr handle)

    let public maximise handle =
        ShowWindowAsync(WindowHandle.ptr handle, 3)
        |> ignore
        
    let public minimise handle =
        ShowWindowAsync(WindowHandle.ptr handle, 6)
        |> ignore
        
    let public restore handle =
        ShowWindowAsync(WindowHandle.ptr handle, 9)
        |> ignore
