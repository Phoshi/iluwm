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
    let average t = total t / 2.0
    
    let multiply (x: float32) w =
        create
            (horizontal w * (float x))
            (vertical w * (float x))
            
    let divide (x: float32) w =
        create
            (horizontal w / (float x))
            (vertical w / (float x))
    
    let private cap f =
        if f < 0.0 then 0.0 else f
    
    let add a b =
        let h = (cap (horizontal a + horizontal b))
        let v = (cap (vertical a + vertical b))
        
        if h > 0. && v > 0. then
            create h v
        else
            b
