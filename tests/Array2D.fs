module Array2D
open Fuchu
open System.Drawing
open FsharpMyExtension
open FsharpMyExtension.Either

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
let leftTrimTests =
    testList "leftTrimTests" [
        testCase "empty" <| fun () ->
            let input =
                [|
                    [| |]
                    [| |]
                |]
            let exp =
                [|
                    [||]
                    [||]
                |]
            let act =
                Array2D.trimLeft ((=) 0) input

            Assert.Equal("", exp, act)

        testCase "one empty" <| fun () ->
            let input =
                [|
                    [| 0 |]
                    [| 0 |]
                |]
            let exp =
                [|
                    [||]
                    [||]
                |]
            let act =
                Array2D.trimLeft ((=) 0) input

            Assert.Equal("", exp, act)

        testCase "not empty" <| fun () ->
            let input =
                [|
                    [| 0 |]
                    [| 1 |]
                |]
            let exp =
                [|
                    [| 0 |]
                    [| 1 |]
                |]
            let act =
                Array2D.trimLeft ((=) 0) input

            Assert.Equal("", exp, act)

        testCase "left and right empty" <| fun () ->
            let input =
                [|
                    [| 0; 1; 0 |]
                    [| 0; 2; 0 |]
                |]
            let exp =
                [|
                    [| 1; 0 |]
                    [| 2; 0 |]
                |]
            let act =
                Array2D.trimLeft ((=) 0) input

            Assert.Equal("", exp, act)
    ]
