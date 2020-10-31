namespace Integration.Rules

open Twime

module Rule =
    type T = TwimeRoot.Update
    
    let applyRules (rules: T list) root =
        let mutable r = Some root
        
        for rule in rules do
            r <- Option.bind rule r
            
        r
        

