namespace Twime

open Microsoft.FSharpLu.Json

module Tree =
    let mapDisplays f  =
        TwimeRoot.map f
        
    let mapTags f =
        TwimeRoot.map (Display.map f)
        
    let mapLayouts f =
        TwimeRoot.map (Display.map (Tag.map f))
        
    let private _maybeMap pred f a =
        if pred a then
            f a
        else
            Some a
        
    let mapDisplay pred f root =
        if TwimeRoot.displays root |> List.exists pred then 
            mapDisplays (_maybeMap pred f) root
        else
            None
    let mapTag pred f =
        mapTags (_maybeMap pred f) 
    let mapLayout pred f =
        mapLayouts (_maybeMap pred f)
        
    let hasDisplay pred tree =
        (TwimeRoot.displays tree)
        |> List.exists pred
        
    let hasTag pred tree =
        hasDisplay
            (fun d ->
                (Display.tags d)
                |> List.exists pred
            )
            tree
            
    let hasLayout pred tree =
        hasTag
            (fun t ->
                pred (t.Layout)
            )
            tree
            
    let windows tree =
        TwimeRoot.displays tree
        |> List.collect Display.tags
        |> List.map Tag.layout
        |> List.collect LayoutTree.windows
    
    let apply transformations (tree: TwimeRoot.T) =
        let mutable t = Some tree
        for transformation in transformations do
            t <- match t with
                    | Some tr -> transformation tr
                    | _ -> None
                    
        t
        
    let save tree =
        Compact.serialize tree
        
    let load string =
        let redoRefs (tree: TwimeRoot.T) =
            let mutable refMapping = []
            let newRef ref =
                List.tryFind (fun (o, _) -> o = ref) refMapping
                |> Option.map (fun (_, n) -> n)
                |> Option.defaultWith (fun () ->
                    let newRef = TreeReference.create ()
                    refMapping <- (ref, newRef) :: refMapping
                    newRef
                    )
                
            let fWin (r, w) =
                LayoutTree.WindowNode (newRef r, w)
                
            let fContainer (r, c, children) =
                let _fContainer (c: Container.T) =
                    Container.withLastActive (Option.map newRef c.LastActiveChild) c
                LayoutTree.ContainerNode (newRef r, _fContainer c, children)
                
            let fLayout l =
                Some(
                    LayoutTree.cataTree
                        fWin
                        fContainer
                        l)
                
            let fTag t =
                Tag.map fLayout t
                |> Option.map (fun t -> {t with Tag.T.Reference = newRef t.Reference})
            
            let fDisplay d =
                let activeTag = Display.activeTag d |> Tag.name
                Display.map fTag d
                |> Option.bind (Display.setActiveTag (Tag.byTagName activeTag))
                |> Option.map (fun mat -> {mat with Display.T.Reference = newRef mat.Reference})
                    
            mapDisplays fDisplay tree
            
        Compact.deserialize string
        |> redoRefs
        
        
    
        
    module Tests =
        open LayoutTree.Tests
        open Layout
        open Layout.Tests
        open NUnit.Framework
        open FsUnit
        
        let rect x y w h = Box.create x y (x + w) (y + h)
        let d name area tags = Display.create (TreeReference.create()) (Display.createMeta name area area false false) tags (List.item 0 tags |> Tag.ref)
        let t name layout = Tag.create (TreeReference.create()) (Tag.createMeta name name None None) layout
            
        [<TestFixture; Category "root">]
        type ``Given a full representative tree`` () =
            let fullTree =
                TwimeRoot.create [
                    d "//DISPLAY1" (rect 0 0 3440 1440) [
                        t "1" (mkTree <| C [W "Firefox"])
                        t "2" (mkTree <| C [])
                    ]
                    d "//DISPLAY2" (rect -1440 -600 1440 2560) [
                        t "1" (mkTree <| V [W "Foobar"; W "Discord"; W "Telegram"])
                    ]
                ] []
            
            [<Test>]
            member x.``I can retrieve and layout the active tab for each display`` () =
                let t = fullTree
                        |> TwimeRoot.displays
                        |> List.collect (fun d ->
                            d
                            |> Display.activeTag
                            |> Tag.layout
                            |> layout (Display.info d).WorkArea BasicNodeReferences.byRoot
                            |> Option.map removeUis
                            |> Option.toList
                            |> List.collect id)
                let e = [
                                            win "Firefox" <| Box.create 0 0 3440 1440
                                            win "Foobar" <| Box.create -1440 -600 0 253
                                            win "Discord" <| Box.create -1440 253 0 1106
                                            win "Telegram" <| Box.create -1440 1106 0 1960
                                        ]
                t
                |> should equal e
