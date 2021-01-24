module IPC.CommandBinding

open Twime

type T =
    {
        command: string
        binding: Binding
    }
and Arguments = string list
and Binding =
    | TreeUpdate of (Arguments -> TwimeRoot.Update)
    | Nop

let create name binding =
    {command = name; binding = binding}
    
let simple binding _ =
    binding
    
let command b = b.command
let binding b = b.binding

let exec b a r =
        match b.binding with
        | TreeUpdate bind -> bind a r
        | _ -> None

let isCommand c b =
    (command b) = c
    
let inline (=>) name binding = create name (TreeUpdate binding)

let argument (args: Arguments) index =
    args.[index]
    
//most significant argument
let msa args = argument args 0

let normalise arg =
    let lower (s: string) = s.ToLower()
    let trim (s: string) = s.Trim()
    
    arg
    |> lower
    |> trim