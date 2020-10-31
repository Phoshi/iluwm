namespace Twime

module TreeReference =
    type T =
        | TreeRef of int64
    let refMaker () =
        let _maker () = 
            let mutable ref = 0L
            let maker () =
                ref <- ref + 1L
                TreeRef ref
            maker
            
        _maker()
        
    let creator = refMaker ()
    let create () = creator ()
    let zero = TreeRef 0L
    let fromLiteral l = TreeRef l
