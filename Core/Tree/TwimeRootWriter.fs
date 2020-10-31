namespace Twime

open LayoutTreeWriter

module TreeWriter =
    let writeTag (t: Tag.T) d =
        let writeMeta (m: Tag.Meta) _d =
            sprintf "(Meta %s)" m.Name
            |> sp _d
        sprintf "(Tag %s %s)" (writeMeta t.Meta (d + 1)) (writeTree t.Layout (d + 1))
        |> sp d
        
    let writeDisplay (disp: Display.T) d =
        let writeMeta (m: Display.Meta) _d =
            sprintf
                "(Meta %s %s)"
                (sp (_d + 1) m.Name)
                (writeBox m.WorkArea (_d + 1))
            |> sp _d
        sprintf
            "(Display %s [%s] %s)"
            (writeMeta disp.Meta (d+1))
            (writeList disp.Tags writeTag (d+1))
            (writeRef disp.ActiveTag (d + 1))
        |> sp d
        
        
    let write (r: TwimeRoot.T) =
        sprintf "(Twime [%s])" (writeList r.Displays writeDisplay 1)
        |> sp 0

