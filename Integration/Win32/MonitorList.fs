namespace Integration.Win32

open NUnit.Framework
open System
open System.Runtime.InteropServices
open Twime

module MonitorList =
    [<AutoOpen>]
    module private Native =
        type MonitorHandle = MonitorHandle of IntPtr
        type MonitorEnumProc = delegate of IntPtr * IntPtr * IntPtr * IntPtr -> bool
        [<DllImport "USER32.dll">]
        extern bool EnumDisplayMonitors(IntPtr hdc, IntPtr lprcClip, MonitorEnumProc lpfnEnum, IntPtr dwData)
        let enumDisplayMonitors f =
            let mutable monitors = []
            if EnumDisplayMonitors(
                                    IntPtr.Zero,
                                    IntPtr.Zero,
                                    MonitorEnumProc(fun handle _ _ _ ->
                                        monitors <- (f (MonitorHandle handle)) :: monitors
                                        true
                                        ),
                                    IntPtr.Zero
                )
            then
                monitors
            else
                []
                
        [<Struct; StructLayout(LayoutKind.Sequential)>]
        type Rect = struct
            val left: int32
            val top: int32
            val right: int32
            val bottom: int32
        end
                
        [<Struct; StructLayout (LayoutKind.Sequential, CharSet = CharSet.Auto, Pack = 4)>]
        type MonitorInfo = struct
            val mutable cbSize: uint32 
            val rcMonitor: Rect
            val rcWork: Rect
            val dwFlags: uint32
            [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 32)>]
            val mutable szDevice: char[]
        end
                          
                           
        [<DllImport("USER32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
        extern [<MarshalAs(UnmanagedType.Bool)>] bool GetMonitorInfo(IntPtr, MonitorInfo&)
        let toBox (rect: Rect) =
            Box.create rect.left rect.top rect.right rect.bottom
        
        let meta handle =
            let mutable monitorInfo = MonitorInfo(szDevice = Array.zeroCreate 32)
            monitorInfo.cbSize <- uint32 (Marshal.SizeOf monitorInfo)
            match handle with
            | MonitorHandle ptr ->
                if GetMonitorInfo(ptr, &monitorInfo) then
                    ()
                else failwith (Marshal.GetLastWin32Error() |> string)
            
            let name =
                monitorInfo.szDevice
                |> Array.filter ((<>) Char.MinValue)
                |> (fun chars -> String.Join("", chars))
                
            let primary = monitorInfo.dwFlags = 1u
                
            (monitorInfo.rcWork |> toBox, name, primary)
            
    type T = {
        name: string
        workArea: Box.T
    }
    
    let monitors () =
        enumDisplayMonitors meta
        
    
    module Tests =
        open FsUnit
        
        [<TestFixture; Category "win32">]
        type ``Given I am running on the current active system`` () =
            [<Test; Ignore "Reliant on system state">]
            member x.``I have at least one monitor attached`` () =
                let monitors =
                    enumDisplayMonitors meta
                            
                
                monitors
                |> List.length
                |> should equal 0
                
