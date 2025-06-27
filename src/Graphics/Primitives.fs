[<RequireQualifiedAccess>]
module FsharpMyExtension.Grahics.Primitive
open System.Drawing

open FsharpMyExtension

let regularPolygon (centerY, centerX) r phi n =
    let pi = System.Math.PI
    let k = float n
    let y i = centerY + r * sin(phi + 2.0 * pi * i / k)
    let x i = centerX + r * cos(phi + 2.0 * pi * i / k)
    [| for i = 0.0 to k - 1.0 do yield x i, y i |]

let regularPolygonR (r: Rectangle) phi n =
    if r.Width = r.Height then
        let k = float r.Width / 2.
        regularPolygon (float r.X + k, float r.Y + k) (float k) phi n
    else
        let mul = float r.Width / float r.Height
        let k = float r.Width / 2.
        regularPolygon (float r.X + k, float r.Y + k) k phi n
        |> Array.map (mapSnd (fun x -> x / mul))

let test () =
    let f () =
        use bmp = new Bitmap(300, 300)
        use g = Graphics.FromImage bmp
        let draw =
            Array.map (mapBoth float32 >> PointF)
            >> fun xs -> g.DrawPolygon(Pens.Chocolate, xs)
        // g.DrawArc(Pens.Aqua, 10, 10, 82, 82, 0, 360)
        let r = 102 |> fun x -> Rectangle(0, 0, 51 * 2 + 1, 30 * 2 + 1)

        regularPolygonR r (System.Math.PI / 2.) 6 |> draw

        g.DrawArc(Pens.Red, r, 0.f, 360.f)
        // regularPolygon (51., 51.) 41. (System.Math.PI / 3.) 5
        // |> draw
        // regularPolygon (51., 51.) 51. (System.Math.PI / 2.) 6
        // |> draw
        // 102. / 2.
        g.DrawRectangle(Pens.DarkRed, r)
        bmp.Save "output.png"

    let x y = System.Math.PI * y / 180.
    System.Math.PI / 3.

    x 60.
    f()
