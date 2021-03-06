namespace Twime.LayoutUIComponents

module UIComponent =

    open Twime

    type TabsConfiguration = {
        directChildren: LayoutTree.T list
    }
    
    let stringifyMarks (marks: string list) =
        let isMarkForUi (m: string) =
            if m.Length > 1 then false
            else true
            
        marks
        |> List.filter isMarkForUi
        |> List.sort
        |> List.rev
        |> List.tryHead
        |> Option.defaultValue ""
        
    let subtitle node =
        match node with
        | LayoutTree.T.WindowNode (_, wi) -> stringifyMarks wi.Definition.marks 
        | _ -> ""
    
    let rec tabName node =
        match node with
        | LayoutTree.T.WindowNode (_, wi) ->
            sprintf "%s" wi.Definition.title
        | LayoutTree.T.ContainerNode (_, ci, ch) ->
            sprintf "%s[%s]" (ci.LayoutEngine.Substring(0, 1).ToUpper()) (List.map tabName ch |> String.concat " | ")
            
    let rec tabActive node =
        match node with
        | LayoutTree.T.WindowNode (_, wi) -> wi.Definition.active
        | LayoutTree.T.ContainerNode (_, _, ch) -> List.exists tabActive ch
    
    
    type UIConfiguration =
        | Tabs of TabsConfiguration
        
    type T = {
        box: Box.T
        container: LayoutTree.T
        config: UIConfiguration
        isRoot: bool
        isPrimary: bool
    }
    
    let box t = t.box
    let withBox box t =
        {t with box = box}
    let container t = t.container
    let config t = t.config
    let isRoot t = t.isRoot
    let isPrimary t = t.isPrimary
    
        
    let tabs container box root primary items =
        {
            box = box
            container = container
            config = Tabs {
                directChildren = items
            }
            isRoot = root
            isPrimary = primary
         }

