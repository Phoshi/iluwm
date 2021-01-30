namespace Twime

open System


module Window =
    module WindowHandle =
        type T = WindowHandle of IntPtr
    module Definition =
        [<StructuredFormatDisplay "(Window {size} (Title {title}) (Minimised {minimised})">]
        type T = {
            size: Box.T
            weight: Weight.T
            minSize: Box.T
            ignoreForHigherOrderLayout: bool
            floating: bool
            processName: string
            title: string
            minimised: bool
            maximised: bool
            zen: bool
            marks: string list
            active: bool
            selected: bool
            lastActiveTrackedWindow: bool
            handle: WindowHandle.T
        }
        
        let size a =
            a.size
            
        let minSize a = a.minSize
            
        let title a =
            a.title
            
        let processName a =
            a.processName
            
        let minimised a =
            a.minimised
            
        let maximised a =
            a.maximised
            
        let zen a =
            a.zen
            
        let marks a =
            a.marks;
            
        let active a =
            a.active
            
        let selected a =
            a.selected
            
        let lastActive a =
            a.active
            
        let weight a =
            a.weight
            
        let handle a =
            a.handle
            
        let withActive active (t: T) =
            {t with active = active}
            
        let withSelected s t =
            {t with selected = s}
            
        let addMark m t =
            let _addMark existing =
                m :: existing
                |> List.distinct
                
            {t with marks = _addMark t.marks}
            
        let removeMark m t =
            {t with marks = List.filter ((<>) m) t.marks}
            
        let removeMarks f t =
            {t with marks = List.filter (fun m -> not (f m)) t.marks}
            
        let withMinSize box t =
            {t with minSize = box}
            
        let create size weight title processName minimised maximised active handle =
            {
                size = size
                weight = weight
                minSize = Box.zero
                title = title
                ignoreForHigherOrderLayout = false
                floating = false
                processName = processName
                minimised = minimised
                maximised = maximised
                zen = false
                marks = []
                active = active
                selected = active
                lastActiveTrackedWindow = active
                handle = handle
            }
            
    [<StructuredFormatDisplay "(Window (Name {Name}) {Definition})">]
    type T = {Name: string; Definition: Definition.T}
    
    let create name definition = {Name = name; Definition = definition}
    
    let name t = t.Name
    let weight t = t.Definition.weight
    let definition t = t.Definition
    
    let withDefinition f (t: T) =
        {t with Definition = (f (definition t))}

