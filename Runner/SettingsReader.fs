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
        
    let stringifyError (e: FSharpErrorInfo) =
        if e.FileName = "unknown" then
            sprintf "%s (%s:%i)" e.Message e.FileName e.Start.Line 
        else
            let lines = System.IO.File.ReadAllLines e.FileName
            
            let errorPos =
                let lead = String.replicate e.Start.Column " "
                let err = String.replicate (e.End.Column - e.Start.Column) "^"
                sprintf "%s%s" lead err 
            
            sprintf "%s (%s:%i)\n\t%s\n\t%s" e.Message e.FileName e.Start.Line (lines.[e.Start.Line-1].Trim()) errorPos
        
        
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
                    "-r:iluwm.dll"
                |])
                |> Async.RunSynchronously
            if exitCode1 > 0 then
                let errs =
                    errors1
                    |> Array.map stringifyError
                    |> String.concat "\n\n"
                failwith errs
            ()
            
        readSettingsFromAssembly outputFile
