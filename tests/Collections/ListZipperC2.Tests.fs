module FsharpMyExtension.Collections.ListZipperCircle2.LZC.Tests
open Fuchu

open FsharpMyExtension.Collections.ListZipperCircle2

[<Tests>]
let RemoveRTest =
    let rem = removeR >> Option.get
    testList "LZC.RemoveR" [
        testCase "base case" <| fun () ->
            Assert.Equal("", None, ofList [1] |> removeR)
        testCase "remove first elem of two" <| fun () ->
            let x, y = 1, 2
            let act = ofList [x;y] |> rem
            Assert.Equal("", ofList [y] |> next, act)
        testCase "remove 2/2" <| fun () ->
            let x, y = 1, 2
            let act = ofList [x;y] |> next |> rem
            let e = ofList [x] |> next
            Assert.Equal("", e, act)
        testCase "remove 2/3" <| fun () ->
            let x, y, z = 1, 2, 3
            let act = ofList [x;y;z] |> next |> rem
            let e = ofList [x;z] |> next
            Assert.Equal("", e, act)
        testCase "remove 3/3" <| fun () ->
            let x, y, z = 1, 2, 3
            let act = ofList [x;y;z] |> next |> next |> rem
            let e = ofList [x;y] |> next |> next
            Assert.Equal("", e, act)
    ]

let removeLTest =
    let rem = removeL >> Option.get
    testList "LZC.RemoveL" [
        testCase "base case" <| fun () ->
            Assert.Equal("", None, ofList [1] |> removeL)
        testCase "remove first elem of two" <| fun () ->
            let x, y = 1, 2
            let act = ofList [x;y] |> rem
            Assert.Equal("", ofList [y] |> prev, act)
        testCase "remove 2/2" <| fun () ->
            let x, y = 1, 2
            let act = ofList [x;y] |> prev |> rem
            let e = ofList [x] |> prev
            Assert.Equal("", e, act)
        testCase "remove 2/3" <| fun () ->
            let x, y, z = 1, 2, 3
            let act = ofList [x;y;z] |> prev |> rem
            let e = ofList [x;z] |> prev
            Assert.Equal("", e, act)
        testCase "remove 3/3" <| fun () ->
            let x, y, z = 1, 2, 3
            let act = ofList [x;y;z] |> prev |> prev |> rem
            let e = ofList [x;y] |> prev |> prev
            Assert.Equal("", e, act)
    ]
