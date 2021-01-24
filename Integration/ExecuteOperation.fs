namespace Integration
open System.Diagnostics

module ExecuteOperation =
    let execute (path: string) (args: string) tree =
        Process.Start(path, args)
        |> ignore
        
        None

