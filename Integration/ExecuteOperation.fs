namespace Integration
open System.Diagnostics

module ExecuteOperation =
    let execute path args tree =
        Process.Start(path, args)
        |> ignore
        
        None

