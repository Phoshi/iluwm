namespace Logume

open System
open System.ComponentModel
open System.Diagnostics

type Context = {
    Class: string
}

type Message = {
    Message: string
    Context: Context option
    Gauges: (string * Object) list
    Time: DateTime
}

type LogLevel =
    | Everything
    | Verbose
    | Errors
    | Nothing

type Log =
    | Trivial of Message
    | Debug of Message
    | Warning of Message
    | Error of Message * Exception
    | Fatal of Message

module Message =
    let message message =
        {
            Message = message
            Context = None
            Time = DateTime.Now
            Gauges = []
        }
        
    let withContext ctx msg =
        {msg with Context = Some ctx}
        
    let addGauge name value msg =
        {msg with Gauges = (name, value) :: msg.Gauges}
        
    let gauges (msg: Message) =
        msg.Gauges
        
    let trivial msg = Trivial msg
    let debug msg = Debug msg
    let warning msg = Warning msg
    let error exn message = Error (message, exn)
    let fatal msg = Fatal msg
        
        
module Log =
    let level l =
        match l with
        | Trivial _ -> "Trivial"
        | Debug _ -> "Debug"
        | Warning _ -> "Warning"
        | Error _ -> "Error"
        | Fatal _ -> "Fatal"
        
    let message l =
        match l with
        | Trivial m -> m
        | Debug m -> m
        | Warning m -> m
        | Error (m, _) -> m
        | Fatal m -> m
        
    let exn l = 
        match l with
        | Trivial m -> None
        | Debug m -> None
        | Warning m -> None
        | Error (m, e) -> Some e
        | Fatal m -> None
        
    let withContext ctx l =
        match l with
        | Trivial m -> Trivial <| Message.withContext ctx m
        | Debug m -> Debug <| Message.withContext ctx m
        | Warning m -> Warning <| Message.withContext ctx m
        | Error (m, e) -> Error (Message.withContext ctx m, e)
        | Fatal m -> Fatal <| Message.withContext ctx m
        
