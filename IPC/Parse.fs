module IPC.Parse
open CommandBinding

let parse (str: string) : (string * Arguments) =
    let split = str.Split(' ')
    
    (split.[0], split |> Array.skip 1 |> List.ofArray)
    
module Tests =
    open FsUnit
    open NUnit.Framework
    
    [<TestFixture>]
    type ``Given a command parser`` () =
        [<Test>]
        member x.``I can parse a command with no parameters`` () =
            parse "commandName"
            |> should equal ("commandName", [])
            
        [<Test>]
        member x.``I can parse a command with a simple parameter`` () =
            parse "commandName param"
            |> should equal ("commandName", ["param"])
            
        [<Test>]
        member x.``I can parse a command with an array parameter`` () =
            parse "commandName [\"param\"; \"list\"]"
            |> should equal ("commandName", ["[\"param\"; \"list\"]"])
