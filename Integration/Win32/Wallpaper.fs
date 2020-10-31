namespace Integration.Win32

open System.Drawing
open System.Drawing
open System.Drawing.Drawing2D
open System.Drawing.Imaging
open System.IO
open System.Runtime.InteropServices
open System.Runtime.InteropServices
open Logume
open Microsoft.Win32
open Twime

module Wallpaper =
    module private Native =
        let SPI_SETDESKWALLPAPER = 20;
        let SPIF_UPDATEINIFILE = 0x01;
        let SPIF_SENDWININICHANGE = 0x02
        
        [<DllImport("user32.dll", CharSet = CharSet.Auto)>]
        extern int SystemParametersInfo(int uAction, int uParam, string lpvParam, int fuWinIni)
        
        let setSpanMode () =
            let key = Registry.CurrentUser.OpenSubKey("Control Panel\\Desktop", true)
            key.SetValue(@"WallpaperStyle", "22");
            key.SetValue(@"TileWallpaper", "0")
            
        let mutable currentWallpaper : string option = None
        
        let setWallpaper path =
            if (currentWallpaper |> Option.contains path |> not) then
                setSpanMode ()
                currentWallpaper <- Some path
                System.Windows.Application.Current.Dispatcher.InvokeAsync(
                    fun () ->
                        SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, System.IO.Path.GetFullPath(path), 0)
                        |> ignore)
                |> ignore
            
    let canSet wallpapers =
        List.forall (fun (_, w) -> Option.isSome w) wallpapers
        
    let createImage box =
        new Bitmap(Box.width box, Box.height box)
        
    let drawWallpaper box wall image =
        use graphics = Graphics.FromImage image
        graphics.CompositingMode <- CompositingMode.SourceCopy
        graphics.InterpolationMode <- InterpolationMode.HighQualityBicubic
        graphics.CompositingQuality <- CompositingQuality.HighQuality
        
        graphics.DrawImage(wall, Box.left box, Box.top box, Box.width box, Box.height box)
       
    let loadImage path =
        use stream = new FileStream(path, FileMode.Open, FileAccess.Read)
        new Bitmap(stream)
        
    let coordinateMapping boxen =
        let left = boxen |> List.map Box.left |> List.min
        let top = boxen |> List.map Box.top |> List.min
        
        Box.translate -left -top
        
    let fullWorkspace boxen =
        let left = boxen |> List.map Box.left |> List.min
        let top = boxen |> List.map Box.top |> List.min
        let right = boxen |> List.map Box.right |> List.max
        let bottom = boxen |> List.map Box.bottom |> List.max
        
        Box.create left top right bottom
        |> coordinateMapping boxen
        
    let boxen wallpapers =
        wallpapers |> List.map (fun (b,_) -> b)
    let wallpapers wallpapers: string list =
        wallpapers |> List.map (fun (_,w) -> w |> Option.get)
        
    let filename walls =
        let wallpaperNames = wallpapers walls
        
        let namePart =
            wallpaperNames
            |> List.map (fun n -> System.IO.Path.GetFileNameWithoutExtension(n))
            |> String.concat "-"
        
        namePart.ToString() + ".jpg"
        
    let set wallpapers =
        if not (canSet wallpapers) then
            ()
            
        let workspace = fullWorkspace (boxen wallpapers)
        let coordinateMapper = coordinateMapping (boxen wallpapers)
        
        if (not (System.IO.File.Exists(filename wallpapers))) then
            use image = createImage workspace
            
            for (box, wallpaper) in wallpapers do
                use wallpaperImage = Option.get wallpaper |> loadImage
                let wallpaperPosition = coordinateMapper box
                
                drawWallpaper wallpaperPosition wallpaperImage image
                
            image.Save(filename wallpapers, ImageFormat.Jpeg)
        
        Native.setWallpaper (filename wallpapers)

