module FsharpMyExtension.Collections.ListZipper.Tests
open Fuchu

open FsharpMyExtension.Collections

[<Tests>]
let RemoveRTest =
    let next = ListZipper.next >> Option.get
    let rem = ListZipper.removeR >> Option.get
    testList "RemoveRTest" [
        testCase "base case" <| fun () ->
            Assert.Equal("", None, ListZipper.ofList [1] |> ListZipper.removeR)
        testCase "remove first elem of two" <| fun () ->
            let x, y = 1, 2
            let act = ListZipper.ofList [x;y] |> rem
            Assert.Equal("", ListZipper.ofList [y], act)
        testCase "remove second elem of two" <| fun () ->
            let x, y = 1, 2
            let act = ListZipper.ofList [x;y] |> next |> rem
            Assert.Equal("", ListZipper.ofList [x], act)
        testCase "remove 2/3" <| fun () ->
            let x, y, z = 1, 2, 3
            let act = ListZipper.ofList [x;y;z] |> next |> rem
            let e = ListZipper.ofList [x;z] |> next
            Assert.Equal("", e, act)
        testCase "remove 3/3" <| fun () ->
            let x, y, z = 1, 2, 3
            let act =
                ListZipper.ofList [x;y;z] |> next |> next |> rem
            let e = ListZipper.ofList [x;y] |> next
            Assert.Equal("", e, act)
    ]

[<Tests>]
let mapiTest =
    testList "mapiTest" [
        testCase "base" <| fun () ->
            let input = [0..9]
            let lz = ListZipper.ofList input
            let act =
                lz
                |> ListZipper.next |> Option.get
                |> ListZipper.mapi (fun i x -> i)
                |> ListZipper.toList
            Assert.Equal("", input, act)
    ]

[<Tests>]
let mapStartMidEndTest =
    testList "mapStartMidEndTest" [
        testCase "base" <| fun () ->
            let lz = ListZipper.ofList [1..4]
            let act =
                lz
                |> ListZipper.next |> Option.get
                |> ListZipper.mapStartMidEnd
                    (fun isCurrent x -> -1)
                    (fun isCurrent x  -> 0)
                    (fun isCurrent x -> 1)
                |> ListZipper.toList
            let exp = [-1; 0; 0; 1]
            Assert.Equal("", exp, act)
    ]
