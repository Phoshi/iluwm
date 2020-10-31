namespace Runner

open System.IO
open System.Reflection
open FSharp.Compiler.Interactive.Shell
open FSharp.Compiler.SourceCodeServices
open Integration
open Twime
open Twime.Bar
open Twime.LayoutPostProcessors
open Twime.UI

module SettingsReader =
    
    let pullSettings (asm: Assembly) =
        asm.GetTypes() |> Seq.pick (fun ty ->
            match ty.GetProperty("settings", [||]) with
            | null -> None
            | mi -> Some (mi.GetValue(null) :?> Settings.T))
        
    let readSettingsFromAssembly path =
        path
        |> Path.GetFullPath
        |> Assembly.LoadFile
        |> pullSettings
        
    let shouldRecompileSettings filename =
        let mtime file =
            File.GetLastWriteTime file
        let outputFile = Path.ChangeExtension(filename, ".dll")
        
        if not (File.Exists outputFile) then
            true
        else if (mtime filename) > (mtime outputFile) then
            true
        else false
        
    let load filename =
        System.Console.WriteLine("Starting up with " + filename)
        let checker = FSharpChecker.Create()
        
        let outputFile = Path.ChangeExtension(filename, ".dll")
        
        if shouldRecompileSettings filename then
            System.Console.WriteLine("Recompile needed on " + filename)
            let errors1, exitCode1 =
                checker.Compile([|
                    "fsc.exe"
                    "-o"; outputFile
                    "-a"; filename
                    "-r:Integration.dll"
                    "-r:Core.dll"
                    "-r:Runner.dll"
                |])
                |> Async.RunSynchronously
            if exitCode1 > 0 then
                let errs =
                    errors1
                    |> Array.map (fun e -> e.Message)
                    |> String.concat "; "
                failwith errs
            ()
            
        readSettingsFromAssembly outputFile
