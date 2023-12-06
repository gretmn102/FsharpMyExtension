module FsharpMyExtension.ArrayArray.Tests
open Fuchu

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
                trimLeft ((=) 0) input

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
                trimLeft ((=) 0) input

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
                trimLeft ((=) 0) input

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
                trimLeft ((=) 0) input

            Assert.Equal("", exp, act)
    ]
