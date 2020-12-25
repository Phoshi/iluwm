namespace Twime

open System.Drawing

module Box =
    [<StructuredFormatDisplay "({Left}, {Top}) => ({Right}, {Bottom})">]
    type T = {Left: int; Right: int; Top: int; Bottom: int}
    let create left top right bottom = {Left = left; Top = top; Bottom = bottom; Right = right}
    let width box = box.Right - box.Left
    let height box = box.Bottom - box.Top
    
    let left box = box.Left
    let right box = box.Right
    let top box = box.Top
    let bottom box = box.Bottom
    
    let horizontalMidpoint box =
        left box + (width box / 2)
        
    let verticalMidpoint box =
        top box + (height box / 2)
            
    let midpoint box =
        horizontalMidpoint box,
        verticalMidpoint box
        
    let contains (x, y) box =
        left box <= x
            && x <= right box
            && top box <= y
            && y <= bottom box
            
    let within outer inner =
        left outer <= left inner
        && right outer >= right inner
        && top outer <= top inner
        && bottom outer >= bottom inner;
    
    let add l t r b box =
        create
            (left box + l)
            (top box + t)
            (right box + r)
            (bottom box + b)
            
    let addBox b1 =
        add
            (left b1)
            (top b1)
            (right b1)
            (bottom b1)
            
    let translate x y box =
        add x y x y box
    
    let fromRect (r: Rectangle) =
        create r.Left r.Top r.Right r.Bottom
        
    let toString (b: T) =
        sprintf
                "(%i, %i) => (%i, %i)"
                (left b)
                (top b)
                (right b)
                (bottom b)
        
        
    let zero =
        create 0 0 0 0
