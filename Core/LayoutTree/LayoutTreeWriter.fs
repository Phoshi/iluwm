namespace Twime

open System

module LayoutTreeWriter =
    let sp d trailing =
        let leading = String.replicate (d * 2) " "
        sprintf "\r\n%s%s" leading trailing
        
        
    let writeList (l: 'a list) (f: 'a -> int -> string) d =
        sprintf "%s" (String.Join(", ", l |> List.map f |> List.map (fun l -> l d)))
        
    let writeBox (b: Box.T) d =
        sprintf "(Box (%i, %i) -> (%i, %i))" b.Left b.Top b.Right b.Bottom
        |> sp d
        
    let writeRef (r: TreeReference.T) d =
        sprintf "%A" r
        |> sp d
        
    let writeWeight (w: Weight.T) d =
        sprintf "(Weight %f %f)" w.Horizontal w.Vertical
        |> sp d
        
    let writeWindow (w: Window.T) d =
        let _writeMeta (m: Window.Definition.T) d =
            sprintf "(Meta %s %s %s %s)"
                (writeBox m.size (d + 1))
                (writeWeight m.weight (d + 1))
                (sp (d + 1) m.title)
                (sp (d + 1) (if m.minimised then "Minimised" else "Visible"))
            |> sp d
        sprintf "(Window %s)" (_writeMeta w.Definition (d + 1))
        |> sp d
        
    let writeContainer (c: Container.T) d =
        sprintf "(LayoutEngine %s)" c.LayoutEngine
        |> sp d
        
    let rec writeTree (t: LayoutTree.T) d =
        let _writeWindow ((_, win): LayoutTree.WindowNode) d =
            sprintf "(Window %s)" (writeWindow win (d + 1))
            |> sp d
            
        let _writeContainer (_, container, children) d =
            sprintf "(Container %s [%s])" (writeContainer container (d + 1)) (writeList children writeTree (d + 1))
            |> sp d
        (match t with
        | LayoutTree.T.WindowNode wn -> _writeWindow wn
        | LayoutTree.T.ContainerNode cn -> _writeContainer cn)
            (d + 1)

