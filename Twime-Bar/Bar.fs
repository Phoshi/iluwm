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
    let setPosition box (ui: Window) =
        ui.Left <- Box.left box |> float
        ui.Top <- Box.top box |> float
        ui.Width <- Box.width box |> float
        ui.Height <- Box.height box |> float
        
    let setItems root runner contents uic (b: Bar) =
        let toDock align =
            match align with
            | Styling.Right -> Some Dock.Right
            | Styling.Left -> Some Dock.Left
            | Styling.Inline -> None
            
        let applyStyles (styling: Styling.T) (control: UserControl) =
            let parseColour colour =
                let bc = BrushConverter()
                (bc.ConvertFrom(colour) :?> Brush)
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
        

