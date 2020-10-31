namespace Twime

open System.Drawing
open FsUnit
open NUnit.Framework

module TagMapping =
    type T = {
        display: Display.Meta
        tags: Tag.Meta list
    }
    
    let display t = t.display
    let tags t = t.tags
    
    let containing tag ts =
        ts
        |> List.tryFind (fun mapping -> tags mapping |> List.exists (Tag.byName tag))
    
    let create d t = {display = d; tags = t}

module TwimeRoot =
    [<StructuredFormatDisplay "(Twime {Displays})">]
    type T =
        {
            Displays: Display.T list
            tags: TagMapping.T list
        }
        
    type Update = T -> T option
        
    let displays t =
        t.Displays
        
    let tags t = t.tags
        
    let display f t =
        t.Displays
        |> List.find f
        
    let private _mapTags f display =
        Display.map f display
        
    let private _mapTrees f (tag: Tag.T) =
        Tag.map f tag
        
    let tagMappingFor display t =
        tags t
        |> List.tryFind display
        
    let tagsFor display t =
        tagMappingFor display t
        |> Option.map TagMapping.tags
        
    let tagById display id t =
        tagsFor display t
        |> Option.bind (List.tryFind (Tag.byName id))
            
    let create displays tags = {Displays = displays; tags = tags}
    
    let map f t =
        let mappedDisplays = displays t
                                |> List.map f
        mappedDisplays
        |> Maybe.lift
        |> Option.map (fun ds -> create ds (tags t))
    
