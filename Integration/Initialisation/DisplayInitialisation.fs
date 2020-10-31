namespace Integration.Initialisation

open Integration.Win32
open Integration
open Logume
open Twime

module DisplayInitialisation =
    let toTwimeDisplay log (settings: Settings.T) (area, name, primary) =
        Message.message ("Creating monitor record for " + name)
        |> Message.debug
        |> log
        
        let tag meta =
            Tag.create
                (TreeReference.create ())
                meta
                (LayoutTree.T.ContainerNode
                     (TreeReference.create (), (Container.create (settings.initialLayout area name primary)), []))
                      
        let display =
            let displayMeta = 
                (Display.createMeta
                     name
                     ((settings.gaps
                       |> Option.bind (fun g -> g.outer)
                       |> Option.map (Box.addBox)
                       |> Option.defaultValue (Box.addBox Box.zero))
                       area)
                     area
                     primary
                     false)
            let tag = settings.tags displayMeta |> List.map tag |> List.head
            
            Display.create
                (TreeReference.create ())
                displayMeta
                [tag]
                tag.Reference
                
        display
                
    let displays log settings =
        MonitorList.monitors ()
        |> List.map (toTwimeDisplay log settings)
        
    let tagDefinitions (settings: Settings.T) displays =
        let toMapping dm =
            TagMapping.create dm (settings.tags dm)
        displays
        |> List.map Display.info
        |> List.map toMapping
