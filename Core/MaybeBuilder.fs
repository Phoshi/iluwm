namespace Twime

[<AutoOpen>]
module Maybe =
    type MaybeBuilder() =
        member this.Bind(x, f) =
            match x with
            | Some a -> f a
            | None -> None
        
        member this.Return(x) = Some x
        member this.ReturnFrom(x) = x
        
    let maybe = new MaybeBuilder()
    
    let lift (lst: 'a option list) : 'a list option =
        if (List.contains None lst) then None
        else Some (List.map Option.get lst)
    

