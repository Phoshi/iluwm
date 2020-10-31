namespace Integration.Win32

open System.Runtime.InteropServices
open System
open Twime

module WindowMove =
    [<AutoOpen>]
    module private DeferWindowPosNatives =
        type T = {handle: IntPtr}
        
        let create handle = {handle = handle}
        let handle t = t.handle
        
        [<DllImport "User32.dll">]
        extern IntPtr BeginDeferWindowPos(int n)
        
        [<DllImport "User32.dll">]
        extern IntPtr DeferWindowPos(
            IntPtr hWinPosInfo,
            IntPtr hWnd,
            IntPtr hWndInsertAfter,
            int x,
            int y,
            int width,
            int height,
            uint32 flags)
        
        [<DllImport "User32.dll">]
        extern bool EndDeferWindowPos(IntPtr hWinPosInfo)
        
        type Flags =
            | SWP_HIDEWINDOW = 0x0080u
            | SWP_NOACTIVATE = 0x0010u
            | SWP_NOCOPYBITS = 0x0100u
            | SWP_NOMOVE = 0x0002u
            | SWP_NOSIZE = 0x0001u
            | SWP_NOZORDER = 0x0004u
            | SWP_SHOWWINDOW = 0x0040u
            
        let moveFlags =
            Flags.SWP_NOZORDER ||| Flags.SWP_NOACTIVATE ||| Flags.SWP_NOCOPYBITS
            |> uint32
        
        let showFlags = Flags.SWP_SHOWWINDOW |> uint32
        let hideFlags = Flags.SWP_HIDEWINDOW |> uint32
        
        let beginBatch n = BeginDeferWindowPos(n) |> create
        let endBatch t = EndDeferWindowPos(handle t)
        
        let _batch flags t windowHandle box =
            DeferWindowPos(
                              handle t,
                              WindowHandle.ptr windowHandle,
                              IntPtr.Zero,
                              Box.left box,
                              Box.top box,
                              Box.width box,
                              Box.height box,
                              flags
                          )
            |> create
            
        let batchMove = _batch moveFlags
        let batchHide = _batch hideFlags 
        let batchShow = _batch showFlags
        
        
    [<AutoOpen>]
    module private Native =
        [<DllImport "User32.dll">]
        extern bool MoveWindow(IntPtr hWnd, int x, int y, int width, int height, bool bRepaint)
        
        [<DllImport "User32.dll">]
        extern bool ShowWindow(IntPtr hWnd, int nCmdShow)
        
        [<DllImport "USER32.dll">]
        extern IntPtr SendMessage(IntPtr hWnd, uint32 Msg, IntPtr wParam, IntPtr lParam)
        
        let WM_ENTERSIZEMOVE = 0x0231u
        let WM_EXITSIZEMOVE = 0x0232u
        
        
        let moveWindow (handle: Window.WindowHandle.T) (box: Box.T) =
            MoveWindow((WindowHandle.ptr handle), (Box.left box), (Box.top box), (Box.width box), (Box.height box), true)
            |> ignore
            
        let enterSizeMove handle =
            SendMessage((WindowHandle.ptr handle), WM_ENTERSIZEMOVE, IntPtr.Zero, IntPtr.Zero)
            |> ignore
            
        let exitSizeMove handle =
            SendMessage((WindowHandle.ptr handle), WM_EXITSIZEMOVE, IntPtr.Zero, IntPtr.Zero)
            |> ignore
            
        let showNoActivate handle =
            ShowWindow(WindowHandle.ptr handle, 4)
            |> ignore
        
            
            
    let private adjustForInvisibleBorders (win: Window.Definition.T) (pos: Box.T) =
        let withoutBorders =
            DwmInfo.extendedWindowBorders (WindowHandle.fromWindow win)
            
        let withBorders =
            WindowSize.size (WindowHandle.fromWindow win)
            
        let offset f =
            (f withBorders) - (f withoutBorders)
            
        let leftOffset = offset Box.left 
        let topOffset = offset Box.top
        let rightOffset = offset Box.right
        let bottomOffset = offset Box.bottom
        
        Box.create
            ((Box.left pos) + leftOffset)
            ((Box.top pos) + topOffset)
            ((Box.right pos) + rightOffset)
            ((Box.bottom pos) + bottomOffset)
            
    
    let batchVisibilityChange (visible: (Window.Definition.T * Box.T) list) (invisible: Window.Definition.T list) =
        let mutable batch = beginBatch ((List.length visible) + (List.length invisible))
        
        for (win, pos) in visible do
            batch <- batchShow batch (WindowHandle.fromWindow win) (adjustForInvisibleBorders win pos)
            
        for win in invisible do
            batch <- batchHide batch (WindowHandle.fromWindow win) (win.size)
            
        endBatch batch |> ignore
            
    let move (w: Window.Definition.T) box =
        moveWindow
            (WindowHandle.fromWindow w)
            box
            
    let batchMove (windows: ((Window.Definition.T * Box.T) list)) =
        let mutable batch = beginBatch (List.length windows)
        
        for (win, pos) in windows do
            enterSizeMove (WindowHandle.fromWindow win)
            batch <- batchMove batch (WindowHandle.fromWindow win) (adjustForInvisibleBorders win pos)
            
        endBatch batch |> ignore
            
    let moveWindows (windows: ((Window.Definition.T * Box.T) list)) =
        for (win, pos) in windows do
            enterSizeMove (WindowHandle.fromWindow win)
            move win (adjustForInvisibleBorders win pos)
            showNoActivate (WindowHandle.fromWindow win)
            exitSizeMove (WindowHandle.fromWindow win)
    
