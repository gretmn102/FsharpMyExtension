module FsharpMyExtension.Primitives.String.Tests
open Fuchu

[<Tests>]
let chunkBySizeTests =
    testList "chunkBySizeTests" [
        testCase "1, abcde" <| fun () ->
            let act =
                chunkBySize 1 "abcde"
            let exp =
                [|"a"; "b"; "c"; "d"; "e"|]
            Assert.Equal("", exp, act)

        testCase "2, abcd" <| fun () ->
            let act =
                chunkBySize 2 "abcd"
            let exp =
                [|"ab"; "cd"|]
            Assert.Equal("", exp, act)

        testCase "2, abcde" <| fun () ->
            let act =
                chunkBySize 2 "abcde"
            let exp =
                [|"ab"; "cd"; "e"|]
            Assert.Equal("", exp, act)
    ]
