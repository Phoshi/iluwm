﻿namespace Integration

open System
open NUnit.Framework

module Hotkey =
    type T =
        | A
        | B
        | C
        | D
        | E
        | F
        | G
        | H
        | I
        | J
        | K
        | L
        | M
        | N
        | O
        | P
        | Q
        | R
        | S
        | T
        | U
        | V
        | W
        | X
        | Y
        | Z
        | D0
        | D1
        | D2
        | D3
        | D4
        | D5
        | D6
        | D7
        | D8
        | D9
        | Equals
        | Minus
        | BracketOpen
        | BracketClose
        | Comma
        | Period
        | Return
        | Up
        | Down
        | Left
        | Right
        | Shift of T
        | Control of T
        | Alt of T
        | Windows of T
        
    let (^+) (a: T -> T) (b: T) = a b
        
    let rec toKeyStroke (t: T): Keystroke.T list =
        match t with
        | Shift andKey -> Keystroke.T.LShiftKey :: toKeyStroke andKey
        | Control andKey -> Keystroke.T.LControlKey :: toKeyStroke andKey
        | Alt andKey -> Keystroke.T.Alt :: toKeyStroke andKey
        | Windows andKey -> Keystroke.T.LWin :: toKeyStroke andKey
        | Comma -> [Keystroke.T.OemComma]
        | Period -> [Keystroke.T.OemPeriod]
        | Equals -> [Keystroke.T.OemPlus]
        | Minus -> [Keystroke.T.OemMinus]
        | BracketOpen -> [Keystroke.T.OemOpenBrackets]
        | BracketClose -> [Keystroke.T.OemCloseBrackets]
        | otherKey -> [Enum.Parse(typeof<Keystroke.T>, otherKey.ToString()) :?> Keystroke.T]
        
    module Tests =
        open FsUnit
        [<TestFixture; Category "Hotkey">]
        type ``Given I am integrating with Windows keystrokes`` () =
            [<Test>]
            member x.``Simple letters translate to integration keystrokes`` () =
                A
                |> toKeyStroke
                |> should equal [Keystroke.T.A]
                
            [<Test>]
            member x.``Control-simple letter translates to a keystroke and modifier`` () =
                Control ^+ A
                |> toKeyStroke
                |> should equal [Keystroke.T.LControlKey; Keystroke.T.A]
                
            [<Test>]
            member x.``Complicated keystrokes translate to keystrokes and modifiers`` () =
                Windows ^+ Shift ^+ J
                |> toKeyStroke
                |> should equal [Keystroke.T.LWin; Keystroke.T.LShiftKey; Keystroke.T.J]
