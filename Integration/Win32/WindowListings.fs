namespace Integration.Win32

open System
open System.Runtime.InteropServices
open Twime
open WindowStyle

module WindowList =
    [<AutoOpen>]
    module private Native =
        type EnumWindowsProc = delegate of IntPtr * int -> bool
        [<DllImport "USER32.DLL">]
        extern bool EnumWindows(EnumWindowsProc enumFunc, int lParam)
    
    let private enumerateTopLevelWindows f =
        let mutable windows = []
        if EnumWindows(EnumWindowsProc(fun handle lparam ->
            windows <- (f (WindowHandle.create handle)) :: windows
            true), 0)
        then
            windows
        else
            []
            
    let private windowShouldBeConsidered handle =
        let className = WindowMetadata.className handle
        
        ["Progman"; "Button"]
        |> List.contains className
        |> not
        
    let private isVisible = WindowStyle.has WS_VISIBLE
    let private isAppWindow = WindowStyle.has WS_EX_APPWINDOW
    let private isToolWindow = WindowStyle.has WS_EX_TOOLWINDOW
    let private isDrawable = has WS_EX_NOREDIRECTIONBITMAP
    let private isActivatable w = not <| has WS_EX_NOACTIVATE w
    let private isResizable = has WS_SIZEBOX
    let private isMaximisable = has WS_MAXIMISEBOX
    
    let private hasTitle w = (WindowMetadata.title w <> "")
            
    let private isUnownedTopLevelWindow handle =
        if isAppWindow handle then
            true
        else if (not (WindowMetadata.hasOwner handle)) && (not (isToolWindow handle)) then
            true
        else
            false
            
    let allWindows () =
        enumerateTopLevelWindows (id)
        |> List.where windowShouldBeConsidered
        |> List.where isVisible
        |> List.where isUnownedTopLevelWindow
        |> List.where isActivatable
        |> List.where isMaximisable
        |> List.where hasTitle
        
        
    module Tests =
        open FsUnit
        open NUnit.Framework
        
        
        [<TestFixture; Category "win32">]
        type ``Given I am running on the current active system`` () =
            [<Test>]
            member x.``asdf`` () = 
                let windows =
                    allWindows ()
                    
                let w = windows
                        |> List.map (fun w -> (WindowMetadata.title w, WindowSize.minimumSize w))
                        
                w |> List.length |> should greaterThan 0
                
                            
                
                
