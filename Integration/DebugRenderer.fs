namespace Integration
open Twime.Box
open Twime

module rec DebugRenderer =
    let noRender w = ()
    let render (w: RenderInstructions.T) =
        let monitors = Win32.MonitorList.monitors ()
        
        System.Console.Clear()
        
        for (box, n, _) in monitors do
           printfn "%s" n
           renderMonitor (50, 200) box w
           
           printfn ""
           printfn ""
        
        ()
        
    let windowAt (hscale, vscale) bounds windows x y =
        let nx = (left bounds) + (x * hscale)
        let ny = (top bounds) + (y * vscale)
        
        if nx > right bounds then None
        else if ny > bottom bounds then None
        else
            windows
            |> RenderInstructions.visibleWindows
            |> List.where (fun (w, b) -> contains (nx, ny) b)
            |> List.map (fun (w, b) -> w)
            |> List.tryHead
        
        
    let charForWindow w =
        (Window.Definition.title (Window.definition w)).ToCharArray().[0]
        
    let renderMonitor (hscale, vscale) bounds windows =
        let w = width bounds / hscale
        let h = height bounds / vscale
        
        let windowAtPosition =
            windowAt
                (hscale, vscale)
                bounds
                windows
        
        for y in 0..h do
            for x in 0..w do
                maybe {
                    let! window = windowAtPosition x y
                    let windowToRight = windowAtPosition (x + 1) y
                    let windowBelow = windowAtPosition x (y + 1)
                    
                    if Option.exists ((<>) window) windowToRight then
                        return '|'
                    else if Option.exists ((<>) window) windowBelow then
                        return '-'
                    else return charForWindow window
                }
                |> Option.defaultValue ' '
                |> printf "%c"
                
            printfn ""
            
