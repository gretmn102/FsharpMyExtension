module FsharpMyExtension.ArrayArray.Tests
open Fuchu

open FsharpMyExtension

[<Tests>]
let ``ArrayArray.horizontalExists`` =
    let xss : _ ArrayArray =
        [|
            [| 0; 2; 4 |]
            [| 1; 3; 5 |]
        |]

    let horizontalExists rowIndex columnRange predicate xss =
        let testedValues = ref []
        let exists =
            xss
            |> horizontalExists rowIndex columnRange (fun x ->
                testedValues.Value <- x :: testedValues.contents
                predicate x
            )
        List.rev testedValues.contents, exists

    testList "ArrayArray.horizontalExists" [
        testCase "0 (0, getWidth xss) (fun _ -> false)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> horizontalExists 0 (0, getWidth xss) (fun _ -> false)
            Assert.Equal("exists", false, actExists)
            Assert.Equal("tested values", [ 0; 2; 4 ], actTestedValues)

        testCase "1 (0, getWidth xss) (fun _ -> false)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> horizontalExists 1 (0, getWidth xss) (fun _ -> false)
            Assert.Equal("exists", false, actExists)
            Assert.Equal("tested values", [ 1; 3; 5 ], actTestedValues)

        testCase "0 (0, getWidth xss) ((=) 2)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> horizontalExists 0 (0, getWidth xss) ((=) 2)
            Assert.Equal("exists", true, actExists)
            Assert.Equal("tested values", [ 0; 2 ], actTestedValues)

        testCase "1 (0, getWidth xss) ((=) 3)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> horizontalExists 1 (0, getWidth xss) ((=) 3)
            Assert.Equal("exists", true, actExists)
            Assert.Equal("tested values", [ 1; 3 ], actTestedValues)

        testCase "1 (1, getWidth xss) ((=) 3)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> horizontalExists 1 (1, getWidth xss) ((=) 3)
            Assert.Equal("exists", true, actExists)
            Assert.Equal("tested values", [ 3 ], actTestedValues)
    ]


[<Tests>]
let ``ArrayArray.horizontalForall`` =
    let xss : _ ArrayArray =
        [|
            [| 0; 2; 4 |]
            [| 1; 3; 5 |]
        |]

    let horizontalForall rowIndex columnRange predicate xss =
        let testedValues = ref []
        let exists =
            xss
            |> horizontalForall rowIndex columnRange (fun x ->
                testedValues.Value <- x :: testedValues.contents
                predicate x
            )
        List.rev testedValues.contents, exists

    testList "ArrayArray.horizontalForall" [
        testCase "0 (0, getWidth xss) (fun _ -> true)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> horizontalForall 0 (0, getWidth xss) (fun _ -> true)
            Assert.Equal("for all", true, actExists)
            Assert.Equal("tested values", [ 0; 2; 4 ], actTestedValues)

        testCase "0 (0, getWidth xss) (fun x -> x < 3)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> horizontalForall 0 (0, getWidth xss) (fun x -> x < 2)
            Assert.Equal("for all", false, actExists)
            Assert.Equal("tested values", [ 0; 2 ], actTestedValues)
    ]

[<Tests>]
let ``ArrayArray.verticalExists`` =
    let xss : _ ArrayArray =
        [|
            [| 0; 1 |]
            [| 2; 3 |]
            [| 4; 5 |]
        |]

    let verticalExists columnIndex rowRange predicate xss =
        let testedValues = ref []
        let exists =
            xss
            |> verticalExists columnIndex rowRange (fun x ->
                testedValues.Value <- x :: testedValues.contents
                predicate x
            )
        List.rev testedValues.contents, exists

    testList "ArrayArray.verticalExists" [
        testCase "0 (0, getHeight xss) (fun _ -> false)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> verticalExists 0 (0, getHeight xss) (fun _ -> false)
            Assert.Equal("exists", false, actExists)
            Assert.Equal("tested values", [ 0; 2; 4 ], actTestedValues)

        testCase "1 (0, getHeight xss) (fun _ -> false)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> verticalExists 1 (0, getHeight xss) (fun _ -> false)
            Assert.Equal("exists", false, actExists)
            Assert.Equal("tested values", [ 1; 3; 5 ], actTestedValues)

        testCase "0 (0, getHeight xss) ((=) 2)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> verticalExists 0 (0, getHeight xss) ((=) 2)
            Assert.Equal("exists", true, actExists)
            Assert.Equal("tested values", [ 0; 2 ], actTestedValues)

        testCase "1 (0, getHeight xss) ((=) 3)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> verticalExists 1 (0, getHeight xss) ((=) 3)
            Assert.Equal("exists", true, actExists)
            Assert.Equal("tested values", [ 1; 3 ], actTestedValues)

        testCase "1 (1, getHeight xss) ((=) 3)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> verticalExists 1 (1, getHeight xss) ((=) 3)
            Assert.Equal("exists", true, actExists)
            Assert.Equal("tested values", [ 3 ], actTestedValues)
    ]

[<Tests>]
let ``ArrayArray.verticalForall`` =
    let xss : _ ArrayArray =
        [|
            [| 0; 1 |]
            [| 2; 3 |]
            [| 4; 5 |]
        |]

    let verticalForall columnIndex rowRange predicate xss =
        let testedValues = ref []
        let exists =
            xss
            |> verticalForall columnIndex rowRange (fun x ->
                testedValues.Value <- x :: testedValues.contents
                predicate x
            )
        List.rev testedValues.contents, exists

    testList "ArrayArray.verticalForall" [
        testCase "0 (0, getHeight xss) (fun _ -> true)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> verticalForall 0 (0, getHeight xss) (fun _ -> true)
            Assert.Equal("for all", true, actExists)
            Assert.Equal("tested values", [ 0; 2; 4 ], actTestedValues)

        testCase "0 (0, getHeight xss) (fun x -> x < 3)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> verticalForall 0 (0, getHeight xss) (fun x -> x < 2)
            Assert.Equal("for all", false, actExists)
            Assert.Equal("tested values", [ 0; 2 ], actTestedValues)
    ]

[<Tests>]
let ``ArrayArray.crop`` =
    testList "ArrayArray.crop" [
        testCase "(0, getWidth xss) (1, 3)" <| fun () ->
            let xss : _ ArrayArray =
                [|
                    [| 0; 1 |]
                    [| 2; 3 |]
                    [| 4; 5 |]
                    [| 6; 7 |]
                |]
            let act =
                crop (0, getWidth xss) (1, 3) xss
            let exp =
                xss[1..2]
            Assert.Equal("", exp, act)

        testCase "(1, 3) (0, getHeight xss)" <| fun () ->
            let xss : _ ArrayArray =
                [|
                    [| 0; 2; 4; 6 |]
                    [| 1; 3; 5; 7 |]
                |]
            let act =
                crop (1, 3) (0, getHeight xss) xss
            let exp =
                xss
                |> Array.map (fun xs ->
                    xs[1..2]
                )
            Assert.Equal("", exp, act)

        testCase "(1, 4) (1, 3)" <| fun () ->
            let xss : _ ArrayArray =
                [|
                    [|  0;  1;  2;  3 |]
                    [|  4;  5;  6;  7 |]
                    [|  8;  9; 10; 11 |]
                    [| 12; 13; 14; 15 |]
                |]
            let act =
                crop (1, 4) (1, 3) xss
            let exp =
                [|
                    [| 5;  6;  7 |]
                    [| 9; 10; 11 |]
                |]
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
