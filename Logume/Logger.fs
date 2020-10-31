namespace Logume

open System
open System.Text
open Microsoft.FSharpLu.Json
open Printf

module Logger =
    type Logger = Log -> unit
    
    let formatDisplayMessage (e:Exception) =
        let sb = StringBuilder()
        bprintf sb "\n"
        let delimeter = String.replicate 50 "*"
        let nl = Environment.NewLine
        let rec printException (e:Exception) count =
            if (e.InnerException <> null)
            then printException (e.InnerException) count
            else
                if (count = 1) then bprintf sb "%s%s%s" e.Message nl delimeter
                else bprintf sb "%s%s%d)%s%s%s" nl nl count e.Message nl delimeter
                bprintf sb "%sType: %s" nl (e.GetType().FullName)
                // Loop through the public properties of the exception object
                // and record their values.
                e.GetType().GetProperties()
                |> Array.iter (fun p ->
                    // Do not log information for the InnerException or StackTrace.
                    // This information is captured later in the process.
                    if (p.Name <> "InnerException" && p.Name <> "StackTrace" &&
                        p.Name <> "Message" && p.Name <> "Data") then
                        try
                            let value = p.GetValue(e, null)
                            if (value <> null)
                            then bprintf sb "%s%s: %s" nl p.Name (value.ToString())
                        with
                        | e2 -> bprintf sb "%s%s: %s" nl p.Name e2.Message
                )
                if (e.StackTrace <> null) then
                    bprintf sb "%s%sStackTrace%s%s%s" nl nl nl delimeter nl
                    bprintf sb "%s%s" nl e.StackTrace
                if (e.InnerException <> null)
                then printException e.InnerException (count+1)
        printException e 1
        sb.ToString()
    
    let private prettyString (log: Log) =
        let msg: Message = (Log.message log)
        let exn: Exception option = Log.exn log
        
        let prettyExn = exn |> Option.map formatDisplayMessage |> Option.defaultValue ""
        
        sprintf "[%s]\t\t%s\t%s %s" (Log.level log) (msg.Time.ToString()) msg.Message prettyExn
    
    let prettyObj obj =
        Compact.serialize obj
        
    let prettyGauge (name, obj) =
        sprintf "[%s] = %s" name (prettyObj obj)
        
    let logLines msg =
        seq {
            yield prettyString msg
            
            for gauge in Message.gauges (Log.message msg) do
                yield (prettyGauge gauge)
        }
    let handle path msg =
        printfn "%s" (prettyString msg)
        
        let logfilename = DateTime.Now.ToString("yyyy-MM-dd") + ".log"
        
        System.IO.File.AppendAllLines(path + logfilename, logLines msg)
        
    let squash log = ()
        
    let log path ctx log =
        log
        |> Log.withContext ({Class = ctx})
        |> handle path
        
        