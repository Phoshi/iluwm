namespace Twime.Bar

open Views.Bar

module Clock =
    let makeClock format =
        Clock(format)
        
    let barComponent styling format =
        Component.makeComponent styling (fun _ _ uic -> makeClock format :> TwimeBarComponent)
        


