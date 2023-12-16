module FsharpMyExtension.Collections.ListZ.Tests
open Fuchu

open FsharpMyExtension.Collections

[<Tests>]
let RemoveRTest =
    let next = ListZ.next >> Option.get
    let rem = ListZ.removeR >> Option.get
    testList "RemoveRTest" [
        testCase "base case" <| fun () ->
            Assert.Equal("", None, ListZ.ofList [1] |> ListZ.removeR)
        testCase "remove first elem of two" <| fun () ->
            let x, y = 1, 2
            let act = ListZ.ofList [x;y] |> rem
            Assert.Equal("", ListZ.ofList [y], act)
        testCase "remove second elem of two" <| fun () ->
            let x, y = 1, 2
            let act = ListZ.ofList [x;y] |> next |> rem
            Assert.Equal("", ListZ.ofList [x], act)
        testCase "remove 2/3" <| fun () ->
            let x, y, z = 1, 2, 3
            let act = ListZ.ofList [x;y;z] |> next |> rem
            let e = ListZ.ofList [x;z] |> next
            Assert.Equal("", e, act)
        testCase "remove 3/3" <| fun () ->
            let x, y, z = 1, 2, 3
            let act =
                ListZ.ofList [x;y;z] |> next |> next |> rem
            let e = ListZ.ofList [x;y] |> next
            Assert.Equal("", e, act)
    ]

[<Tests>]
let mapiTest =
    testList "mapiTest" [
        testCase "base" <| fun () ->
            let input = [0..9]
            let lz = ListZ.ofList input
            let act =
                lz
                |> ListZ.next |> Option.get
                |> ListZ.mapi (fun i x -> i)
                |> ListZ.toList
            Assert.Equal("", input, act)
    ]

[<Tests>]
let mapStartMidEndTest =
    testList "mapStartMidEndTest" [
        testCase "base" <| fun () ->
            let lz = ListZ.ofList [1..4]
            let act =
                lz
                |> ListZ.next |> Option.get
                |> ListZ.mapStartMidEnd
                    (fun isCurrent x -> -1)
                    (fun isCurrent x  -> 0)
                    (fun isCurrent x -> 1)
                |> ListZ.toList
            let exp = [-1; 0; 0; 1]
            Assert.Equal("", exp, act)
    ]
