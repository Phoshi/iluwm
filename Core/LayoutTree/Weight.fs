namespace Twime

module Weight =
    [<StructuredFormatDisplay "(Weight {Horizontal}/{Vertical})">]
    type T = {Horizontal: float; Vertical: float}
    
    let create h v = {Horizontal = h; Vertical = v}
    let init = create 1.0 1.0
    let zero = create 0.0 0.0
    
    let horizontal t = t.Horizontal
    let vertical t = t.Vertical
    
    let total t = horizontal t + vertical t
    
    let private cap f =
        if f < 0.0 then 0.0 else f
    
    let add a b
        = create (cap (horizontal a + horizontal b)) (cap (vertical a + vertical b))
