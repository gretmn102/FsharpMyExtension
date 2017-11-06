[<RequireQualifiedAccess>]
module FsharpMyExtension.Bitmap
open FsharpMyExtension.TwoDSeq
open System.Drawing
let private toT (bmp:Bitmap) = {
    TwoDOp.T.Get = fun i j -> bmp.GetPixel(i,j)
    TwoDOp.T.Set = fun i j x -> bmp.SetPixel(i,j, x)
    TwoDOp.T.Length1 = bmp.Width
    TwoDOp.T.Length2 = bmp.Height
}
let iteri f = toT >> TwoDOp.iteri f
/// not working
let iteriP f = toT >> TwoDOp.iterPar f
let mapP f (bmp:Bitmap) =
        // match bmp.PixelFormat with
        // | Imaging.PixelFormat.Format32bppArgb ->
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
            // true
        // | Imaging.PixelFormat.Format24bppRgb ->
        //     let bmpData =
        //         bmp.LockBits(Rectangle(0, 0, bmp.Width, bmp.Height),
        //             Imaging.ImageLockMode.ReadWrite,
        //             Imaging.PixelFormat.Format24bppRgb)
        //     let ptr = bmpData.Scan0
        //     let len = abs bmpData.Stride * bmp.Height

        //     let rgbValues : byte [] = Array.zeroCreate len
        //     System.Runtime.InteropServices.Marshal.Copy(ptr, rgbValues, 0, len)

        //     rgbValues |> Array.chunkBySize 3
        //     |> Array.Parallel.iteri (fun i x ->
        //         let x = x |> Array.map int
        //         match x with
        //         | [| b; g; r; |] ->
        //             let c = Color.FromArgb(r, g, b)
        //             let set =
        //                 let i = i * 3
        //                 fun n -> Array.set rgbValues (i + n)
        //             let (c:Color) = f c
        //             set 0 c.B; set 1 c.G; set 2 c.R;
        //         | _ -> failwith "")
        //     System.Runtime.InteropServices.Marshal.Copy(rgbValues, 0, ptr, len)
        //     bmp.UnlockBits(bmpData)
        //     true
        // | _ -> false
    // ofBitmap bmp
let map f = toT >> TwoDOp.map f

let iterFoldi f st = toT >> TwoDOp.iterFoldi f st
let mapFoldi f st = toT >> TwoDOp.mapFoldi f st

// let map f (bmp:Bitmap) =
    // for i = 0 to bmp.Width - 1 do
    //     for j = 0 to bmp.Height - 1 do
    //         bmp.SetPixel(i, j, f <| bmp.GetPixel(i,j))
// let iteri f (bmp:Bitmap) =
    // for i = 0 to bmp.Width - 1 do
    //     for j = 0 to bmp.Height - 1 do
    //         f i j <| bmp.GetPixel(i,j)
// let mapFoldi f st (bmp:Bitmap) =
    // let mutable st = st
    // for i = 0 to bmp.Width - 1 do
    //     for j = 0 to bmp.Height - 1 do
    //         let c,st' = f st (i, j) (bmp.GetPixel(i,j))
    //         st <- st'
    //         bmp.SetPixel(i, j, c)
    // st
// let iterFoldi f st (bmp:Bitmap) =
//     let mutable st = st
//     for i = 0 to bmp.Width - 1 do
//         for j = 0 to bmp.Height - 1 do
//             let st' = f st (i, j) (bmp.GetPixel(i,j))
//             st <- st'
//     st
