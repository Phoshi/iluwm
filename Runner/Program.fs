open System
open Integration.Rules
open Logume
open Microsoft.FSharpLu.Json
open Runner
open Integration
open Twime
open Twime.TreeOperation
open Twime.TreeAddOperation
open Twime.TreeOperations
open Twime.TreeRemoveOperation
open Twime.TreeUpdateOperation
open Twime.TreeTagOperation
open Twime.TreeFocusChangeOperation
open Twime.SpecialCaseAwareOperation
open Twime.LayoutPostProcessors
open Integration.TreeOperations.LogOperation
open IPC
open EventRunner


let execute (settings: Settings.T) =
    let loggerFor = Logger.log settings.workingDirectoryPath settings.logLevel
    
    let handlerFromConfig handler otherwise firstOf =
        let configured = settings.windowEventHandlers
                            |> Option.bind handler
                            
        if Option.isSome configured then
            (Option.get configured) firstOf
        else
            otherwise
            
    let onWindowCreated (firstOf: WindowHandler) =
        handlerFromConfig
            (fun weh -> weh.created)
            (firstOf {
                    ignoreIfAlreadyOnActiveTag
                    switchToTagIfWindowAlreadyExists
                    logWindow "New window" (loggerFor "LogWindow")
                    handleSpecialCases settings.specialCases
                    addAfterActiveWindow 
                    logWindow "Couldn't find active window..." (loggerFor "LogWindow")
                    addToRootOfClosestDisplay
                    logWindow "Couldn't even find active display..." (loggerFor "LogWindow")
                    addToRootOfPrimaryDisplay
            })
            firstOf
        
    let onWindowDestroyed (firstOf: WindowHandler) =
        handlerFromConfig
            (fun weh -> weh.destroyed)
            (firstOf {
                leaveWindowIfOnlyOnInactiveTag
                leaveWindowIfInZen
                logWindow "Destroyed window" (loggerFor "LogWindow")
                removeWindow
            })
            firstOf
        
    let onWindowResized _ (firstOf: WindowHandler) =
        handlerFromConfig
            (fun weh -> weh.resized)
            (firstOf {
                logWindow "Resized window" (loggerFor "LogWindow")
                updateWindow size
            })
            firstOf
            
    let onTitleChange _ (firstOf: WindowHandler) =
        handlerFromConfig
            (fun weh -> weh.titleChanged)
            (firstOf {
                logWindow "Title change" (loggerFor "LogWindow")
                updateWindow title
            })
            firstOf
            
    let onMinimizeChange (firstOf: WindowHandler) =
        handlerFromConfig
            (fun weh -> weh.minimiseChanged)
            (firstOf {
                logWindow "Minimize change" (loggerFor "LogWindow")
                updateWindow minimised
            })
            firstOf
        
    let onMaximizeChange (firstOf: WindowHandler) =
        handlerFromConfig
            (fun weh -> weh.maximiseChanged)
            (firstOf {
                logWindow "Maximize change" (loggerFor "LogWindow")
                updateWindow maximised
            })
            firstOf
        
    let onActiveChange (firstOf: WindowHandler) =
        handlerFromConfig
            (fun weh -> weh.activeChanged)
            (firstOf {
                logWindow "Active window change" (loggerFor "LogWindow")
                updateTree [updateLastWindowMark; (updateWindowActive); (updateWindowLastActive); (updateSelected); unmarkActiveWindow]
            })
            firstOf
        
    let onActiveDisplayChange (d: string) r =
        updateActiveDisplay d r
        
        
    let availableDisplays = Initialisation.DisplayInitialisation.displays (loggerFor "Displays") settings
        
    let mutable twimeRoot =
        TwimeRoot.create
            availableDisplays 
            (Initialisation.DisplayInitialisation.tagDefinitions settings availableDisplays)
    
    let persist tree =
        let serializedLayout = Tree.save tree
        System.IO.File.WriteAllText(settings.workingDirectoryPath + "last.tree", serializedLayout)
    
    let teardown tree =
        let showWindow ((_, w): LayoutTree.WindowNode) =
            Integration.Win32.WindowMove.batchVisibilityChange
                [(w.Definition, w.Definition.size)]
                []
            ()
        
        tree
        |> TwimeRoot.displays
        |> List.collect Display.tags
        |> List.map Tag.layout
        |> List.map (LayoutTree.cataTree
                        showWindow
                        (fun _ -> ()))
        |> ignore
        
        
    Message.message "Starting up"
    |> Message.debug
    |> loggerFor "Startup"
        
    if System.IO.File.Exists(settings.workingDirectoryPath + "last.tree") then
        let treeToLoad =
            Tree.load (System.IO.File.ReadAllText (settings.workingDirectoryPath + "last.tree"))
            |> Option.bind (fun ttl -> LoadTreeOperation.load ttl twimeRoot)
            
        if Option.isSome treeToLoad then
            twimeRoot <- Option.get treeToLoad
        
        
    let lock = new Object();
    use eventRunner = new EventRunner.T(
                        lock,
                        (loggerFor "EventRunner"),
                        teardown,
                        persist,
                        WindowsMustExist.rule,
                        twimeRoot,
                        onWindowCreated,
                        onWindowDestroyed,
                        onWindowResized,
                        onTitleChange,
                        onMinimizeChange,
                        onMaximizeChange,
                        onActiveChange,
                        onActiveDisplayChange,
                        Compositor.composit
                            settings.uiSize
                            (1280, 1280)
                            (settings.gaps |> Option.bind (fun g -> g.outer))
                            settings.uiType
                            (settings.postProcessors
                             |> Option.bind (fun pp -> pp.window)
                             |> Option.orElse (settings.gaps |> Option.bind (fun g -> g.inner) |> Option.map (fun gapMax -> GapsPostProcessor.postprocessScaling PostProcessor.ProcessingStyle.Window (settings.gaps |> Option.bind (fun g -> g.innerMin) |> Option.defaultValue gapMax) gapMax))
                             |> Option.defaultValue NoPostProcessor.postprocess)
                            (settings.postProcessors
                             |> Option.bind (fun pp -> pp.ui |> Option.orElse pp.window)
                             |> Option.orElse (settings.gaps |> Option.bind (fun g -> g.inner) |> Option.map (fun b -> Box.create (Box.left b) 0 (Box.right b) 0) |> Option.map (GapsPostProcessor.postprocess PostProcessor.ProcessingStyle.UI))
                             |> Option.defaultValue NoPostProcessor.postprocess)
                        ,
                        (BatchRenderer.render
                             (loggerFor "Renderer")
                             settings.workingDirectoryPath
                             settings.uiConfig
                             (BatchRenderer.Settings.create true BatchRenderer.Settings.Batch))
                        )
   
    let app = WpfRigging.beginWpf ()
    
    let poller = Integration.Poller.create
                     (Integration.WindowTracker.tracker
                        lock
                        (loggerFor "Tracker")
                        settings.hotkeys
                        (eventRunner.transformAsync UserDriven)
                        eventRunner.onCreated
                        eventRunner.onDestroyed
                        eventRunner.onResized
                        eventRunner.onTitleChange
                        eventRunner.onMinimize
                        eventRunner.onMaximize
                        eventRunner.onActiveChange
                        eventRunner.onMonitorActiveChange
                        eventRunner.batchComplete
                        eventRunner.forceRender)
                     100.0
                     
    NamedPipeIPCServer.runIpcServer
        (loggerFor "iluwmipc")
        "iluwmipc"
        (NamedPipeIPCServer.handleIncomingCommand
            (settings.hotkeys)
            (eventRunner.transformAsync UserDriven))
    |> ignore
    
        
    NamedPipeIPCServer.runIpcServer
        (loggerFor "iluwmipcquery")
        "iluwmipcquery"
        (NamedPipeIPCServer.handleIncomingQuery
            (settings.hotkeys)
            (fun () -> NamedPipeIPCServer.listWindows eventRunner)
            (fun () -> NamedPipeIPCServer.listTags eventRunner)
            (fun () -> NamedPipeIPCServer.listMarks eventRunner))
    |> ignore
                     
    WpfRigging.runWpf app |> ignore
                     
    Integration.MessagePump.pumpQueue ()
    
    0 // return an integer exit code

[<EntryPoint; STAThread>]
let main argv =
    let settings = SettingsReader.load "Settings.fsx"
    execute settings
    
    
