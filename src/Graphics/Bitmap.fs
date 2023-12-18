[<RequireQualifiedAccess>]
module FsharpMyExtension.Grahics.Bitmap
open System.Drawing

open FsharpMyExtension.Collections

let private toT (bmp: Bitmap) : TwoDSeq<_, _> = {
    Get = fun i j -> bmp.GetPixel(i, j)
    Set = fun i j x -> bmp.SetPixel(i, j, x)
    Length1 = bmp.Width
    Length2 = bmp.Height
}
let iteri f = toT >> TwoDSeq.iteri f
let map f = toT >> TwoDSeq.map f
let iterFoldi f st = toT >> TwoDSeq.iterFoldi f st
let mapFoldi f st = toT >> TwoDSeq.mapFoldi f st

/// not working
let iteriP f = toT >> TwoDSeq.Parallel.iter f

// https://github.com/thomerow/png2bmp32/blob/master/ImageConverter.cs#L95
let mapP f (bmp:Bitmap) =
    let bmpData =
        bmp.LockBits(Rectangle(0, 0, bmp.Width, bmp.Height),
            Imaging.ImageLockMode.ReadWrite,
            Imaging.PixelFormat.Format32bppArgb)
    let ptr = bmpData.Scan0
    let len = abs bmpData.Stride * bmp.Height

    let rgbValues : byte [] = Array.zeroCreate len
    System.Runtime.InteropServices.Marshal.Copy(ptr, rgbValues, 0, len)

    rgbValues |> Array.chunkBySize 4
    |> Array.Parallel.iteri (fun i x ->
        let x = x |> Array.map int
        match x with
        | [| b; g; r; a; |] ->
            let c = Color.FromArgb(a, r, g, b)
            let set =
                let i = i * 4
                fun n -> Array.set rgbValues (i + n)
            let (c:Color) = f c
            set 0 c.B; set 1 c.G; set 2 c.R; set 3 c.A
        | _ -> failwith "")
    System.Runtime.InteropServices.Marshal.Copy(rgbValues, 0, ptr, len)
    bmp.UnlockBits(bmpData)

let ofArray (width:int) height xss =
    let bmp = new System.Drawing.Bitmap(width, height)

    for i = 0 to width - 1 do
        for j = 0 to height - 1 do
            bmp.SetPixel(i, j, Array.get (Array.get xss j) i)
    bmp
let ofArray' xss =
    let width = Array.length (Array.item 0 xss)
    let height = Array.length xss
    ofArray width height xss

let toArray (img : System.Drawing.Bitmap) =
    let w, h = img.Width, img.Height
    let xss: System.Drawing.Color [] [] =
        Array.create h [||]
        |> Array.map (fun _ -> Array.create w System.Drawing.Color.Empty)
    for i = 0 to w - 1 do
        for j = 0 to h - 1 do
            xss.[j].[i] <- img.GetPixel(i, j)
    xss
assert
    let w,h = 3, 5
    let xss =
        List.chunkBySize 3 [1..w * h]
        |> List.map (List.map System.Drawing.Color.FromArgb >> Array.ofList)
        |> Array.ofList
    let bmp = ofArray w h xss
    bmp |> toArray = xss
