namespace Integration

open System
open System.Threading
open System.Windows
open System.Windows.Threading
open Integration.Win32
open Integration.Win32
open Twime

module public Poller =
    type T = {Timer: DispatcherTimer;}
    
    let _create timer  = {Timer = timer}
        
    let public create f interval =
        let timer = new System.Windows.Threading.DispatcherTimer()
        timer.Interval <- TimeSpan.FromMilliseconds(interval)
        timer.Tick.Add(fun _ -> f())
        timer.Start()
        
        _create timer 

module public WindowTracker =
    type TrackedWindow = Window.Definition.T * Window.WindowHandle.T
    type T = {windowList: TrackedWindow list; activeDisplay: string; firstRun: bool}
    let create wl display = {windowList = wl; activeDisplay = display; firstRun = false}
    let public initState = {windowList = []; activeDisplay = ""; firstRun = true}
    let wl t = t.windowList
    let display t = t.activeDisplay
    
    let tracked handle =
        ( Window.Definition.create
            (DwmInfo.extendedWindowBorders handle)
            Weight.init
            (WindowMetadata.title handle)
            (WindowMetadata.executableName handle)
            (WindowState.isMinimised handle)
            (WindowState.isMaximised handle)
            (WindowState.isForeground handle)
            handle,
            handle)
    
    let public stateMaintainer l (state: T) f =
        let mutable s = ref state
        (fun _ ->
            lock l (fun () -> 
                let currentState = Volatile.Read(s)
                Volatile.Write(s, f currentState)))
        
    let _sameWindow (_, a) (_, b) =
        a = b
        
        
    let changedWindows oldWindows newWindows (f: Window.Definition.T -> 'a) =
        newWindows
        |> List.filter (fun w -> List.exists (_sameWindow w) (wl oldWindows))
        |> List.map (fun w -> (w, List.find (_sameWindow w) (wl oldWindows)))
        |> List.filter (fun ((a, _), (b, _)) -> not <| (f a = f b))
    
    let activeDisplay () =
        let displays = MonitorList.monitors ()
        let mousePosition = CursorMove.getCursorPosition ()
        
        maybe {
            let! activeMonitorInformation = List.tryFind
                                                (fun (b, _, _) -> Box.contains mousePosition b)
                                                displays
            let name = match activeMonitorInformation with (_, n, _) -> n
            
            return name
        }
                                        
                                        
    let public tracker lock log hotkeys eventRunner onNewWindow onDestroyedWindow onResizedWindow onRetitledWindow onMinimisedWindow onMaximisedWindow onActiveWindowChanged onActiveMonitorChange onBatchComplete onStartup =
        let _tracker state =
            if state.firstRun then
                onStartup()
                
                create state.windowList state.activeDisplay
            else 
          
            let allWindows =
                 WindowList.allWindows ()
                 |> List.filter (fun w -> not <| WindowState.isMinimised w)
                 |> List.map tracked
            
            let changed f = changedWindows state allWindows f
            
            let d = activeDisplay ()
            
            if (wl state) = allWindows && (display state |> Some = d) then
                state
            else
                let newWindows =
                    allWindows
                    |> List.filter (fun w -> not <| List.exists (_sameWindow w) (wl state))
                    
                let oldWindows =
                    (wl state)
                    |> List.filter (fun w -> not <| List.exists (_sameWindow w) allWindows)
                    
                for (w, _) in newWindows do
                    onNewWindow w |> ignore
                    if w.active then onActiveWindowChanged w
                    
                for (w, _) in oldWindows do
                    onDestroyedWindow w |> ignore
             
                for ((newWindow, _), (oldWindow, _)) in changed Window.Definition.size do
                    onResizedWindow oldWindow.size newWindow
                    
                for ((newWindow, _), (oldWindow, _)) in changed Window.Definition.title do
                    onRetitledWindow oldWindow.title newWindow
                    
                for ((newWindow, _), (oldWindow, _)) in changed Window.Definition.minimised do
                    onMinimisedWindow newWindow
                    
                for ((newWindow, _), (oldWindow, _)) in changed Window.Definition.maximised do
                    onMaximisedWindow newWindow
                    
                for ((newWindow, _), (oldWindow, _)) in changed Window.Definition.active do
                    onActiveWindowChanged newWindow
                    
                if (display state |> Some) <> d then
                    for monitor in Option.toList d do 
                        onActiveMonitorChange monitor
                        
                onBatchComplete ()
                create allWindows (Option.defaultValue (display state) d)
            
        stateMaintainer lock initState _tracker
