namespace Twime

open System
open System.Windows
open Integration
open Integration.Rules
open Logume
open Twime.TreeOperation

module rec EventRunner =
    type UpdateType =
        | NoUpdate 
        | Maintainance
        | LowPriority
        | HighPriority
        | UiDriven
        | UserDriven
        
    type EventMessage =
        | Transform of TwimeRoot.Update * UpdateType
        
    type WindowHandler = FirstOptionOf<Window.Definition.T>
    type EventHandler = WindowHandler -> TwimeRoot.T option
    type EventHandler<'a> = 'a -> WindowHandler -> TwimeRoot.T option
    
    type Run = (TwimeRoot.T -> TwimeRoot.T option) -> unit
    
    type Compositor = UpdateType -> TwimeRoot.T -> RenderInstructions.T option
    type Renderer = Run -> UpdateType -> RenderInstructions.T -> unit
    
    exception TwimeExit 
    
    type T(
           lockSemaphor: Object,
           logger: Logger.Logger,
           teardown: TwimeRoot.T -> unit,
           persist: TwimeRoot.T -> unit,
           rule: Rule.T,
           root: TwimeRoot.T,
           created: EventHandler,
           destroyed: EventHandler,
           resized: EventHandler<Box.T>,
           titleChange: EventHandler<string>,
           minimised: EventHandler,
           maximised: EventHandler,
           activeChange: EventHandler,
           activeMonitorChange: string -> TwimeRoot.Update,
           compositor: Compositor,
           renderer: Renderer) =
        let mutable _root = root
        let mutable _highestUpdate = UpdateType.NoUpdate
        
        let processor (runner: EventRunner.T) = 
            MailboxProcessor.Start(fun inbox ->
                    let rec loop n =
                        async {
                                let! msg = inbox.Receive()
                                match msg with
                                | Transform (tr, lv) ->
                                    runner.transform lv tr 
                                    runner.render()
                                    return! loop n
                            }
                    loop 0
                )
        
        let shouldRender l =
            match l with
            | NoUpdate -> false
            | _ -> true
        
        let _update l f =
            try
                Message.message (sprintf "Pre-transformation at level %A" l)
                |> Message.addGauge "tree" _root
                |> Message.debug
                |> logger
                
                match _root |> f |> Option.bind rule with
                | Some r ->
                    _root <- r
                    
                    Message.message "Post-transformation"
                    |> Message.addGauge "tree" _root
                    |> Message.debug
                    |> logger
                    
                    persist r
                    
                    if l > _highestUpdate then
                        _highestUpdate <- l
                        
                | None -> 
                    printfn "%s" "Tree update failed"
            with
                | TwimeExit ->
                    teardown _root
                    Environment.Exit(0)
                | ex ->
                    Message.message "Tree update failed"
                    |> Message.addGauge "tree" _root
                    |> Message.error ex
                    |> logger 
                    
        let update l f = lock lockSemaphor (fun () -> _update l f)
        
        let handleEvent l f w =
            let _twimeRootUpdate root =
                f (firstOptionOf root (withParameter w))
            
            update l _twimeRootUpdate
            
        let handleTransform l f =
            let _twimeRootUpdate root =
                f root
            
            update l _twimeRootUpdate
            
        member this.mailbox = processor this
        
        member this.renderCycle l =
            try 
                match compositor l _root with
                | Some output -> renderer (this.transformAsync UiDriven) l output
                | None -> ()
            with
            | ex ->
                Message.message "Render failed"
                |> Message.addGauge "tree" _root
                |> Message.error ex
                |> logger 
            
            
        member this.render () =
            if shouldRender _highestUpdate then
                Message.message "Running render cycle"
                |> Message.addGauge "tree" _root
                |> Message.debug
                |> logger
                
                this.renderCycle _highestUpdate
                
            _highestUpdate <- UpdateType.NoUpdate
            
        member this.forceRender () =
            _highestUpdate <- UpdateType.HighPriority
            
            this.render()
            
            
        member this.onCreated = handleEvent HighPriority created
            
        member this.onDestroyed = handleEvent LowPriority destroyed
        
        member this.onResized oldSize = handleEvent LowPriority (resized oldSize)
        
        member this.onTitleChange oldTitle = handleEvent Maintainance (titleChange oldTitle)
        
        member this.onMinimize = handleEvent LowPriority minimised
        member this.onMaximize = handleEvent LowPriority maximised
        
        member this.onActiveChange = handleEvent Maintainance activeChange
        
        member this.onMonitorActiveChange display = handleTransform Maintainance (activeMonitorChange display)
        
        member this.transform l f = handleTransform l f
        member this.transformAsync l f =
            this.mailbox.Post(Transform (f, l))
            
        member this.batchComplete () = this.render ()
        
        override this.Finalize() =
            teardown _root
            
        interface IDisposable with
            member this.Dispose() =
                teardown root

