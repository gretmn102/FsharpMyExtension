module Array2D
open Fuchu
open System.Drawing
open FsharpMyExtension
open FsharpMyExtension.Either

module Expect =
    let equal act exp msg =
        Assert.Equal(msg, exp, act)

[<Tests>]
let mapPTest =
    testList "mapPTest" [
        testCase "base case" <| fun () ->
            let exp =
                let r = System.Random()
                Array2D.zeroCreate 10 20
                |> Array2D.map (fun _ -> r.Next(0, 100))
            let act = Array2D.copy exp |> fun yss -> Array2D.Parallel.map id yss; yss

            Assert.Equal("", exp, act)
    ]
let sampleBmpArr =
    [|[|Color.Red; Color.Gold; Color.Gray|]
      [|Color.Black; Color.Blue; Color.Brown|]|] |> Array2D.ofArAr

[<Tests>]
let ofBitmapFast =
    let pfs =
        System.Enum.GetValues(typeof<Imaging.PixelFormat>)
        |> Seq.cast<Imaging.PixelFormat>
    let bmps =
        pfs |> Seq.map (fun pf ->
            try
                sampleBmpArr |> Array2D.toBitmapSlow pf
                |> comma pf
                |> Right
            with e -> Left(pf, e)
        )
    let _lefts, rights = bmps |> List.ofSeq |> List.partitionEithers
    let xs =
        rights |> List.map (fun (x, bmp) ->
            let exp = Array2D.ofBitmapSlow bmp
            try
                Array2D.ofBitmapFast bmp
                |> Right
            with e -> Left e
            |> fun act -> x, ((Right exp):Either<exn, _>), act
        )
    // rights |> List.iter (fst >> printfn "%A")
    let ys = xs |> List.map (fun (x, exp, act) ->
        testCase (sprintf "%A" x) <| fun () ->
            Assert.Equal("", exp, act) )
    testList "ofBitmapFastTest'" ys

[<Tests>]
let toBitmapFastTest =
    testList "toBitmapFastTest" [
        testCase "base case" <| fun () ->
            let exp =
                sampleBmpArr
                |> Array2D.toBitmapSlow Imaging.PixelFormat.Format32bppArgb
                |> Array2D.ofBitmapSlow
            let act = sampleBmpArr |> Array2D.toBitmapFast |> Array2D.ofBitmapSlow
            Assert.Equal("", exp, act)
   ]

[<Tests>]
let ``Array2D.toArray`` =
    testList "Array2D.toArray" [
        testCase "empty" <| fun () ->
            Expect.equal
                (Array2D.toArray (Array2D.create 0 0 0))
                [||]
                ""

        testCase "2x2" <| fun () ->
            Expect.equal
                ([|
                    [| 0; 1 |]
                    [| 2; 3 |]
                |]
                |> Array2D.ofArAr
                |> Array2D.toArray)
                [| 0; 1; 2; 3 |]
                ""

        testCase "2x3" <| fun () ->
            Expect.equal
                ([|
                    [| 0; 1 |]
                    [| 2; 3 |]
                    [| 4; 5 |]
                |]
                |> Array2D.ofArAr
                |> Array2D.toArray)
                [| 0; 1; 2; 3; 4; 5 |]
                ""

        testCase "3x2" <| fun () ->
            Expect.equal
                ([|
                    [| 0; 1; 2 |]
                    [| 3; 4; 5 |]
                |]
                |> Array2D.ofArAr
                |> Array2D.toArray)
                [| 0; 1; 2; 3; 4; 5 |]
                ""
    ]
