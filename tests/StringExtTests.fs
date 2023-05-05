module StringExtTests
open FsharpMyExtension
open Fuchu

[<Tests>]
let chunkBySizeTests =
    testList "chunkBySizeTests" [
        testCase "1, abcde" <| fun () ->
            let act =
                String.chunkBySize 1 "abcde"
            let exp =
                [|"a"; "b"; "c"; "d"; "e"|]
            Assert.Equal("", exp, act)

        testCase "2, abcd" <| fun () ->
            let act =
                String.chunkBySize 2 "abcd"
            let exp =
                [|"ab"; "cd"|]
            Assert.Equal("", exp, act)

        testCase "2, abcde" <| fun () ->
            let act =
                String.chunkBySize 2 "abcde"
            let exp =
                [|"ab"; "cd"; "e"|]
            Assert.Equal("", exp, act)
    ]
