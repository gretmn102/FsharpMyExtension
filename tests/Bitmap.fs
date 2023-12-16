module Bitmap
open Fuchu
open System.Drawing

open FsharpMyExtension
open FsharpMyExtension.Collections

[<Tests>]
let MapPTest =
    testList "MapPTest" [
        testCase "base case" <| fun () ->
            let xss =
                [[Color.Red; Color.Gold; Color.Gray]
                 [Color.Black; Color.Blue; Color.Brown]]
                 |> Array2D.ofListListD

            let f g =
                let fn (c:Color) =
                    Color.FromArgb(int c.A / 2, int c.R / 2,
                        int c.G / 2, int c.B / 2 )
                use bmp = xss |> Array2D.toBitmap
                g fn bmp
                bmp |> Array2D.ofBitmapSlow
            let exp = f Bitmap.map
            let act = f (fun f b -> Bitmap.mapP f b |> ignore)
            Assert.Equal("", exp, act)
        // testCase "speed test" <| fun () ->
        //     let xss =
        //         [[Color.Red; Color.Gold; Color.Gray]
        //          [Color.Black; Color.Blue; Color.Brown]]
        //          |> Array2D.ofListListD

        //     let fn (c:Color) =
        //         Color.FromArgb(int c.A / 2, int c.R / 2,
        //             int c.G / 2, int c.B / 2 )
        //     let f g path =
        //         use bmp = new Bitmap(path : string)
        //         //use bmp = xss |> Array2D.toBitmap
        //         if g fn bmp then
        //             bmp |> Array2D.ofBitmapSlow |> Some
        //         else None
        //     let path = @"e:\temp\dalek___doctor_who_by_stickeesbiz-d6fqn80.png"
        //     let path = @"e:\temp\12-Доктор-Доктор-(DW)-Таймлорды-Doctor-Who-4088222.jpeg"
        //     let exp () = f (fun x y -> Bitmap.map x y; true) path
        //     let act () = f Bitmap.mapP path
        //     exp () |> ignore
        //     act () |> ignore
        //     let arr () = Array2D.ofBitmapFast (new Bitmap (path))
        //     let exp () = Array2D.ofBitmapSlow (new Bitmap (path))
        //     arr() |> Option.get = exp()
        //         //|> Option.map (fun b -> Array2D.mapP fn b; b)
        //     arr () |> ignore
        //     act () |> ignore
        //     arr() |> Option.get |> Array2D.toBitmap |> fun x -> x.Save("e:\\dalekArr.png")
        //     exp() |> Array2D.toBitmap |> fun x -> x.Save("e:\\dalekAct.png")
        //     arr() = act()
        //     Assert.Equal("", exp(), act())
   ]
