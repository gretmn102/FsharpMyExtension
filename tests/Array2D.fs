module FsharpMyExtension.Collections.Array2D.Tests
open Fuchu
open System.Drawing

open FsharpMyExtension
open FsharpMyExtension.Either

open Helpers

[<Tests>]
let mapPTest =
    testList "mapPTest" [
        testCase "base case" <| fun () ->
            let exp =
                let r = System.Random()
                Array2D.zeroCreate 10 20
                |> Array2D.map (fun _ -> r.Next(0, 100))
            let act = Array2D.copy exp |> fun yss -> Parallel.map id yss; yss

            Assert.Equal("", exp, act)
    ]

let sampleBmpArr =
    [|[|Color.Red; Color.Gold; Color.Gray|]
      [|Color.Black; Color.Blue; Color.Brown|]|] |> ofArAr

[<Tests>]
let ofBitmapFast =
    let pfs =
        System.Enum.GetValues(typeof<Imaging.PixelFormat>)
        |> Seq.cast<Imaging.PixelFormat>
    let bmps =
        pfs |> Seq.map (fun pf ->
            try
                sampleBmpArr |> toBitmapSlow pf
                |> Pair.create pf
                |> Right
            with e -> Left(pf, e)
        )
    let _lefts, rights = bmps |> List.ofSeq |> List.partitionEithers
    let xs =
        rights |> List.map (fun (x, bmp) ->
            let exp = ofBitmapSlow bmp
            try
                ofBitmapFast bmp
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
                |> toBitmapSlow Imaging.PixelFormat.Format32bppArgb
                |> ofBitmapSlow
            let act = sampleBmpArr |> toBitmapFast |> ofBitmapSlow
            Assert.Equal("", exp, act)
   ]

[<Tests>]
let ``Array2D.toArray`` =
    testList "Array2D.toArray" [
        testCase "empty" <| fun () ->
            Expect.equal
                (toArray (Array2D.create 0 0 0))
                [||]
                ""

        testCase "2x2" <| fun () ->
            Expect.equal
                ([|
                    [| 0; 1 |]
                    [| 2; 3 |]
                |]
                |> ofArAr
                |> toArray)
                [| 0; 1; 2; 3 |]
                ""

        testCase "2x3" <| fun () ->
            Expect.equal
                ([|
                    [| 0; 1 |]
                    [| 2; 3 |]
                    [| 4; 5 |]
                |]
                |> ofArAr
                |> toArray)
                [| 0; 1; 2; 3; 4; 5 |]
                ""

        testCase "3x2" <| fun () ->
            Expect.equal
                ([|
                    [| 0; 1; 2 |]
                    [| 3; 4; 5 |]
                |]
                |> ofArAr
                |> toArray)
                [| 0; 1; 2; 3; 4; 5 |]
                ""
    ]

[<Tests>]
let mapStartMidEndTests =
    testList "forallTest" [
        testCase "one" <| fun () ->
            let xs = Array.zeroCreate 6
            let res =
                ofListListD [[1..3]; [4..6]]
                |> forall (fun x -> xs.[x - 1] <- x; true)

            Assert.Equal("", [|1..6|], xs)
            Assert.Equal("", true, res)
    ]
