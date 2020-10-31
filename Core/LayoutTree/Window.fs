namespace Twime

open System


module Window =
    module WindowHandle =
        type T = WindowHandle of IntPtr
    module Definition =
        [<StructuredFormatDisplay "(Window {size} (Title {title}) (Minimised {minimised})">]
        type T = {size: Box.T; title: string; minimised: bool; maximised: bool; active: bool; lastActiveTrackedWindow: bool; handle: WindowHandle.T}
        
        let size a =
            a.size
            
        let title a =
            a.title
            
        let minimised a =
            a.minimised
            
        let maximised a =
            a.maximised
            
        let active a =
            a.active
            
        let lastActive a =
            a.active
            
        let handle a =
            a.handle
            
        let withActive active (t: T) =
            {t with active = active}
        
        let create size title minimised maximised active handle =
            {
                size = size
                title = title
                minimised = minimised
                maximised = maximised
                active = active
                lastActiveTrackedWindow = active
                handle = handle
            }
            
    [<StructuredFormatDisplay "(Window (Name {Name}) {Weight})">]
    type T = {Name: string; Weight: Weight.T; Definition: Definition.T}
    
    let create name weight definition = {Name = name; Weight = weight; Definition = definition}
    
    let name t = t.Name
    let weight t = t.Weight
    let definition t = t.Definition
    
    let withDefinition f (t: T) =
        {t with Definition = (f (definition t))}

