[<RequireQualifiedAccessAttribute>]
module FsharpMyExtension.Collections.Array2D

open FsharpMyExtension.TwoDSeq
let private toT (xss:_ [,]) = {
    TwoDOp.T.Get = Array2D.get xss
    TwoDOp.T.Set = Array2D.set xss
    TwoDOp.T.Length1 = Array2D.length1 xss
    TwoDOp.T.Length2 = Array2D.length2 xss
}

let iterFoldi f st = toT >> TwoDOp.iterFoldi f st
let mapFoldi f st = toT >> TwoDOp.mapFoldi f st




open FsharpMyExtension

let ofListList l1 l2 xss =
    let yss = Array2D.zeroCreate l1 l2
    xss |> List.iteri (fun i -> Array2D.set yss i |> fun f -> List.iteri f )
    yss
let ofListListD xss =
    xss |> ofListList (List.length xss) (List.head xss |> List.length)

let ofSeqSeq l1 l2 xss =
    let yss = Array2D.zeroCreate l1 l2
    xss |> Seq.iteri (fun i x -> Array2D.set yss i |> fun f -> x |> Seq.iteri f )
    yss
let ofSeqSeqD xss =
    xss |> ofSeqSeq (Seq.length xss) (Seq.head xss |> Seq.length)

let ofArAr xss =
    let ofArAr l1 l2 xss =
        Array2D.init l1 l2 (fun i j ->
            Array.get (Array.get xss i) j
        )
        // let yss = Array2D.zeroCreate l1 l2
        // xss |> Array.iteri (fun i x ->
        //     Array2D.set yss i |> fun f -> x |> Array.iteri f )
        // yss
    xss |> ofArAr (Array.length xss) (Array.get xss 0 |> Array.length)

open System.Drawing
/// Подходит любой из:
/// Format16bppRgb555
/// Format16bppRgb565
/// Format24bppRgb
/// Format32bppRgb
/// Format16bppArgb1555
/// Format32bppPArgb
/// Format48bppRgb
/// Format64bppPArgb
/// Format32bppArgb
/// Format64bppArgb
let ofBitmapFast (bmp:Bitmap) =
    let bmpData =
        bmp.LockBits(Rectangle(0, 0, bmp.Width, bmp.Height),
            Imaging.ImageLockMode.ReadOnly,
            Imaging.PixelFormat.Format32bppArgb)
    let ptr = bmpData.Scan0
    let len = abs bmpData.Stride * bmp.Height

    let rgbValues : byte [] = Array.zeroCreate len
    System.Runtime.InteropServices.Marshal.Copy(ptr, rgbValues, 0, len)
    bmp.UnlockBits(bmpData)

    let colors = Array2D.zeroCreate bmp.Height bmp.Width
    0 |> for' 0 (bmp.Height - 1) (fun st i ->
        st |> for' 0 (bmp.Width - 1) (fun st j ->
            let get n = Array.get rgbValues (st + n) |> int
            let c = Color.FromArgb(get 3, get 2, get 1, get 0)
            Array2D.set colors i j c
            st + 4
        )
    ) |> ignore
    colors

// let ofBitmap32bppArgb (bmp:Bitmap) =
//     if bmp.PixelFormat = Imaging.PixelFormat.Format32bppArgb then
//         ofBitmap32bppArgb' bmp
//     else failwith "bmp is not PixelFormat.Format32bppArgb"
// let ofBitmap24bppRgb (bmp:Bitmap) =
//     let pixFrm = Imaging.PixelFormat.Format24bppRgb
//     if bmp.PixelFormat = pixFrm then
//         let bmpData =
//             bmp.LockBits(Rectangle(0, 0, bmp.Width, bmp.Height),
//                 Imaging.ImageLockMode.ReadOnly,
//                 Imaging.PixelFormat.Format24bppRgb)
//         let ptr = bmpData.Scan0
//         let len = abs bmpData.Stride * bmp.Height

//         let rgbValues : byte [] = Array.zeroCreate len
//         System.Runtime.InteropServices.Marshal.Copy(ptr, rgbValues, 0, len)
//         bmp.UnlockBits(bmpData)

//         let colors = Array2D.zeroCreate bmp.Height bmp.Width

//         let xss = rgbValues |> Array.chunkBySize 3
//         printf "%f\n%d;%d" (float xss.Length / 3.) len (bmp.Height * bmp.Width)
//         // xss |> Array.Parallel.iteri (fun i x ->
//         //     let x = x |> Array.map int
//         //     match x with
//         //     | [| b; g; r; |] ->
//         //         let i, j = i / bmp.Width, i % bmp.Width
//         //         Array2D.set colors i j (Color.FromArgb(r, g, b))
//         //     | _ -> failwith "")
//         // 0 |> for' 0 (bmp.Height - 1) (fun st i ->
//         //     st |> for' 0 (bmp.Width - 1) (fun st j ->
//         //         let get n = Array.get rgbValues (st + n) |> int
//         //         let c = Color.FromArgb(get 2, get 1, get 0)
//         //         Array2D.set colors i j c
//         //         st + 3
//         //     )) |> ignore
//         colors
//     else failwith "bmp is not PixelFormat.Format24bppRgb"

// let ofBitmapFast (bmp:Bitmap) =
//     match bmp.PixelFormat with
//     | Imaging.PixelFormat.Format32bppArgb ->
//         ofBitmap32bppArgb bmp |> Some
//     | Imaging.PixelFormat.Format24bppRgb ->
//         ofBitmap24bppRgb bmp |> Some
//     | _ -> None

let ofBitmapSlow (bmp:Bitmap) =
    Array2D.init bmp.Height bmp.Width (fun i j -> bmp.GetPixel(j,i))
let ofBitmap (bmp:Bitmap) = ofBitmapFast bmp

let toBitmapFast (xss:Color [,]) =
    let bmp = new Bitmap(Array2D.length2 xss, Array2D.length1 xss)
    let bmpData =
        bmp.LockBits(Rectangle(0, 0, bmp.Width, bmp.Height),
            Imaging.ImageLockMode.WriteOnly,
            Imaging.PixelFormat.Format32bppArgb)
    let ptr = bmpData.Scan0
    let len = abs bmpData.Stride * bmp.Height

    let rgbValues =
        let w = Array2D.length2 xss
        Array.Parallel.init len (fun i ->
            let x =
                let i = i / 4
                Array2D.get xss (i / w) (i % w)
            match i % 4 with
            | 3 -> x.A
            | 2 -> x.R
            | 1 -> x.G
            | 0 -> x.B
            | _ -> failwith ""
        )


    // System.Threading.Tasks.Task.Factory.

    // System.Threading.Tasks.TaskFactory()
    // последовательный способ:
    // let rgbValues : byte [] = Array.zeroCreate len
    // iterFoldi (fun st _ (x:Color) ->
    //     let set i = Array.set rgbValues (st + i)
    //     set 3 x.A
    //     set 2 x.R
    //     set 1 x.G
    //     set 0 x.B
    //     st + 4 ) 0 xss |> ignore

    System.Runtime.InteropServices.Marshal.Copy(rgbValues, 0, ptr, len)
    bmp.UnlockBits(bmpData)
    bmp
let toBitmapSlow (imgPixFrm:Imaging.PixelFormat) xss =
    let bmp = new Bitmap(Array2D.length2 xss, Array2D.length1 xss, imgPixFrm)
    Array2D.iteri (fun i j x -> bmp.SetPixel(j, i, x)) xss
    bmp
/// default - Imaging.PixelFormat.Format32bppArgb
let toBitmap =
    // toBitmapF Imaging.PixelFormat.Format32bppArgb
    toBitmapFast


let toArrayArray xss =
    let h = Array2D.length1 xss
    let w = Array2D.length2 xss
    Array.init h (fun i -> Array.init w (fun j -> xss.[i,j] ))

assert
    let xss = [|[| 1..5 |]; [| 6..10 |]|]
    xss
    |> ofArAr
    |> toArrayArray
    |> (=) xss

module Parallel =
    let iteri f = toT >> TwoDOp.Parallel.iter f
    let map f = toT >> TwoDOp.Parallel.map f
    let mapi f = toT >> TwoDOp.Parallel.mapi f
    let toArrayArray xss =
        let h = Array2D.length1 xss
        let w = Array2D.length2 xss

        let yss = Array.init h (fun _ -> Array.zeroCreate w)
        // for i = 0 to Array2D.length1 xss - 1 do
        //     for j = 0 to Array2D.length2 xss - 1 do
        //         yss.[i].[j] <- xss.[i,j]
        iteri (fun i j x -> yss.[i].[j] <- x) xss
        yss
    assert
        let xss = [|[| 1..5 |]; [| 6..10 |]|]
        xss
        |> ofArAr
        |> toArrayArray
        |> (=) xss
    let toArrayArray' xss =
        let h = Array2D.length1 xss
        let w = Array2D.length2 xss
        Array.Parallel.init h (fun i ->
            Array.Parallel.init w (fun j ->
                xss.[i,j]
            )
        )
    assert
        let xss = [|[| 1..5 |]; [| 6..10 |]|]
        xss
        |> ofArAr
        |> toArrayArray'
        |> (=) xss

/// n*i + j
/// Отображение двухмерного массива на одномерный.
/// при `w = 3`:
/// `|0|1|2|`
/// `|3|4|5|`
/// `-> |0|1|2|3|4|5|`
let to1DArray =
    // n * i + j
    fun (w:int) (i:int) -> (+) (w * i)

let testTo1DArray2 () =
    let w, h = 3, 2
    let xss =
        [|
            [| 0; 1; 2 |]
            [| 3; 4; 5 |]
        |]
    let ys = Array.create (w * h) 0
    xss |> Array.iteri (fun i ->
        let f = to1DArray w i
        Array.iteri (fun j x ->
            Array.set ys (f j) x ))
    ys = [|0; 1; 2; 3; 4; 5|]
let testTo1DArray () =
    let len = 6
    let input = [0..len - 1]
    let n = 3
    let xss = input |> List.chunkBySize n |> ofListList (len / n) n
    let act = Array.zeroCreate len
    xss |> Array2D.iteri (fun i j x ->
        to1DArray n i j |> fun i -> Array.set act i x )
    input |> Array.ofList = act

let getWidth = Array2D.length2

let getHeight = Array2D.length1

let toArray (xss: _ [,]) : _ [] =
    let width, height =  getWidth xss, getHeight xss
    let toArrayIndex = to1DArray width
    let ys = Array.zeroCreate (width * height)
    xss
    |> Array2D.iteri (fun i j x ->
        ys[toArrayIndex i j] <- x
    )
    ys

/// i / n, i % n
/// при `w = 3`:
/// `|0|1|2|3|4|5|`
/// `->`
/// `|0|1|2|`
/// `|3|4|5|`
let to2DArray n i = i / n, i % n
let testTo2DArray () =
    let len = 10
    let input = [0..len - 1]
    let n = 2
    let exp = input |> List.chunkBySize n |> ofListList (len / n) n
    let act : int [,] = Array2D.zeroCreate (len / n) n
    input |> List.iteri (fun i x ->
        to2DArray n i |> fun (i,j) ->
            Array2D.set act i j x) //printfn "%d%A" x i)
    exp = act

let ofArray width xs =
    let height = int (ceil (float32 (Array.length xs) / float32 width))
    let xss = Array2D.zeroCreate height width

    xs
    |> Array.iteri (fun i x ->
        xss.[i / width, i % width] <- x
    )
    xss

let forall predicate xss =
    let w, h = Array2D.length2 xss, Array2D.length1 xss
    let rec f y =
        if 0 <= y && y < h then
            let rec g x =
                if 0 <= x && x < w then
                    if predicate xss.[y, x] then
                        g (x + 1)
                    else
                        false
                else
                    f (y + 1)
            g 0
        else
            true
    f 0

let foralli predicate xss =
    let w, h = Array2D.length2 xss, Array2D.length1 xss
    let rec f y =
        if 0 <= y && y < h then
            let rec g x =
                if 0 <= x && x < w then
                    if predicate y x xss.[y, x] then
                        g (x + 1)
                    else
                        false
                else
                    f (y + 1)
            g 0
        else
            true
    f 0
