namespace Twime.Bar

open System.ComponentModel
open System.Windows
open System.Windows.Controls
open System.Windows.Controls
open System.Windows.Controls
open System.Windows.Media
open Twime
open Twime.LayoutUIComponents
open Views.Bar

module Bar =
    let private isSelected uic =
        UIComponent.container uic
        |> LayoutTree.isSelected
        
    let parseColour colour =
        let bc = BrushConverter()
        let b = (bc.ConvertFrom(colour) :?> Brush)
        
        b.Freeze()
        
        b
        
    let setPosition box (ui: Window) =
        let min x y =
            if x < y then y else x
        ui.Left <- Box.left box |> float
        ui.Top <- Box.top box |> float
        ui.Width <- Box.width box |> min 0 |> float
        ui.Height <- Box.height box |> min 0 |> float
        
    let setItems root runner contents uic (b: Bar) =
        let toDock align =
            match align with
            | Styling.Right -> Some Dock.Right
            | Styling.Left -> Some Dock.Left
            | Styling.Inline -> None
            
        let applyStyles (styling: Styling.T) (control: UserControl) =
                
            let toThickness box =
                let dir ection =
                    ection box |> float
                Thickness(dir Box.left, dir Box.top, dir Box.right, dir Box.bottom)
                
            Option.iter (fun back -> control.Background <- parseColour back) styling.background
            Option.iter (fun fore -> control.Foreground <- parseColour fore) styling.foreground
            Option.iter (fun padd -> control.Padding <- toThickness padd) styling.padding
            
            match styling.alignment |> Option.bind toDock with
            | Some dock ->
                control.SetValue(DockPanel.DockProperty, dock)
            | None ->
                control.ClearValue(DockPanel.DockProperty)
            
            ()
            
        let toItem comp =
            let control = Component.contents comp root runner uic
            
            
            applyStyles (Component.styling comp) control
            control
            
        let items =
            contents
            |> List.map (fun c -> toItem c)
        
        b.SetItems(items)
        if isSelected uic then
            b.Background <- parseColour "#123456"
        else 
            b.Background <- parseColour "#3b4252"
        
    let show (b: Bar) =
        b.Show()
        
    let close (b: Bar) =
        b.Close()
        
    let bar (box: Box.T) root runner (contents: Component.T list) (uiComponent: UIComponent.T) =
        let b = Bar()
        
        setPosition box b
        setItems root runner contents uiComponent b
        show b
        
        b
        

