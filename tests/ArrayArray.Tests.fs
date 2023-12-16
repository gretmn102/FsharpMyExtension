module FsharpMyExtension.Collections.ArrayArray.Tests
open Fuchu

open FsharpMyExtension
open FsharpMyExtension.Collections

[<Tests>]
let ``ArrayArray.rowExists`` =
    let xss : _ ArrayArray =
        [|
            [| 0; 2; 4 |]
            [| 1; 3; 5 |]
        |]

    let rowExists rowIndex columnRange predicate xss =
        let testedValues = ref []
        let exists =
            xss
            |> rowExists rowIndex columnRange (fun x ->
                testedValues.Value <- x :: testedValues.contents
                predicate x
            )
        List.rev testedValues.contents, exists

    testList "ArrayArray.rowExists" [
        testCase "0 (0, getWidth xss) (fun _ -> false)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> rowExists 0 (0, getWidth xss) (fun _ -> false)
            Assert.Equal("exists", false, actExists)
            Assert.Equal("tested values", [ 0; 2; 4 ], actTestedValues)

        testCase "1 (0, getWidth xss) (fun _ -> false)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> rowExists 1 (0, getWidth xss) (fun _ -> false)
            Assert.Equal("exists", false, actExists)
            Assert.Equal("tested values", [ 1; 3; 5 ], actTestedValues)

        testCase "0 (0, getWidth xss) ((=) 2)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> rowExists 0 (0, getWidth xss) ((=) 2)
            Assert.Equal("exists", true, actExists)
            Assert.Equal("tested values", [ 0; 2 ], actTestedValues)

        testCase "1 (0, getWidth xss) ((=) 3)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> rowExists 1 (0, getWidth xss) ((=) 3)
            Assert.Equal("exists", true, actExists)
            Assert.Equal("tested values", [ 1; 3 ], actTestedValues)

        testCase "1 (1, getWidth xss) ((=) 3)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> rowExists 1 (1, getWidth xss) ((=) 3)
            Assert.Equal("exists", true, actExists)
            Assert.Equal("tested values", [ 3 ], actTestedValues)
    ]


[<Tests>]
let ``ArrayArray.rowForall`` =
    let xss : _ ArrayArray =
        [|
            [| 0; 2; 4 |]
            [| 1; 3; 5 |]
        |]

    let rowForall rowIndex columnRange predicate xss =
        let testedValues = ref []
        let exists =
            xss
            |> rowForall rowIndex columnRange (fun x ->
                testedValues.Value <- x :: testedValues.contents
                predicate x
            )
        List.rev testedValues.contents, exists

    testList "ArrayArray.rowForall" [
        testCase "0 (0, getWidth xss) (fun _ -> true)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> rowForall 0 (0, getWidth xss) (fun _ -> true)
            Assert.Equal("for all", true, actExists)
            Assert.Equal("tested values", [ 0; 2; 4 ], actTestedValues)

        testCase "0 (0, getWidth xss) (fun x -> x < 3)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> rowForall 0 (0, getWidth xss) (fun x -> x < 2)
            Assert.Equal("for all", false, actExists)
            Assert.Equal("tested values", [ 0; 2 ], actTestedValues)
    ]

[<Tests>]
let ``ArrayArray.columnExists`` =
    let xss : _ ArrayArray =
        [|
            [| 0; 1 |]
            [| 2; 3 |]
            [| 4; 5 |]
        |]

    let columnExists columnIndex rowRange predicate xss =
        let testedValues = ref []
        let exists =
            xss
            |> columnExists columnIndex rowRange (fun x ->
                testedValues.Value <- x :: testedValues.contents
                predicate x
            )
        List.rev testedValues.contents, exists

    testList "ArrayArray.columnExists" [
        testCase "0 (0, getHeight xss) (fun _ -> false)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> columnExists 0 (0, getHeight xss) (fun _ -> false)
            Assert.Equal("exists", false, actExists)
            Assert.Equal("tested values", [ 0; 2; 4 ], actTestedValues)

        testCase "1 (0, getHeight xss) (fun _ -> false)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> columnExists 1 (0, getHeight xss) (fun _ -> false)
            Assert.Equal("exists", false, actExists)
            Assert.Equal("tested values", [ 1; 3; 5 ], actTestedValues)

        testCase "0 (0, getHeight xss) ((=) 2)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> columnExists 0 (0, getHeight xss) ((=) 2)
            Assert.Equal("exists", true, actExists)
            Assert.Equal("tested values", [ 0; 2 ], actTestedValues)

        testCase "1 (0, getHeight xss) ((=) 3)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> columnExists 1 (0, getHeight xss) ((=) 3)
            Assert.Equal("exists", true, actExists)
            Assert.Equal("tested values", [ 1; 3 ], actTestedValues)

        testCase "1 (1, getHeight xss) ((=) 3)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> columnExists 1 (1, getHeight xss) ((=) 3)
            Assert.Equal("exists", true, actExists)
            Assert.Equal("tested values", [ 3 ], actTestedValues)
    ]

[<Tests>]
let ``ArrayArray.columnForall`` =
    let xss : _ ArrayArray =
        [|
            [| 0; 1 |]
            [| 2; 3 |]
            [| 4; 5 |]
        |]

    let columnForall columnIndex rowRange predicate xss =
        let testedValues = ref []
        let exists =
            xss
            |> columnForall columnIndex rowRange (fun x ->
                testedValues.Value <- x :: testedValues.contents
                predicate x
            )
        List.rev testedValues.contents, exists

    testList "ArrayArray.columnForall" [
        testCase "0 (0, getHeight xss) (fun _ -> true)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> columnForall 0 (0, getHeight xss) (fun _ -> true)
            Assert.Equal("for all", true, actExists)
            Assert.Equal("tested values", [ 0; 2; 4 ], actTestedValues)

        testCase "0 (0, getHeight xss) (fun x -> x < 3)" <| fun () ->
            let actTestedValues, actExists =
                xss
                |> columnForall 0 (0, getHeight xss) (fun x -> x < 2)
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
let ``ArrayArray.trim`` =
    testList "ArrayArray.trim" [
        testCase "empty" <| fun () ->
            let input =
                [||]
            let exp =
                None
            let act =
                trim TrimOption.all ((=) 0) input
            Assert.Equal("", exp, act)

        testCase "0x0 empty" <| fun () ->
            let input =
                [|
                    [||]
                    [||]
                |]
            let exp =
                None
            let act =
                trim TrimOption.all ((=) 0) input
            Assert.Equal("", exp, act)

        testCase "0x0 empty2" <| fun () ->
            let input =
                [|
                    [| 0 |]
                    [| 0 |]
                |]
            let act =
                trim TrimOption.all ((=) 0) input
            let exp =
                None
            Assert.Equal("", exp, act)

        testCase "1x1 top empty" <| fun () ->
            let input =
                [|
                    [| 0 |]
                    [| 1 |]
                |]
            let act =
                trim TrimOption.all ((=) 0) input
            let exp =
                Some [|
                    [| 1 |]
                |]
            Assert.Equal("", exp, act)

        testCase "1x1 right empty" <| fun () ->
            let input =
                [|
                    [| 1; 0 |]
                |]
            let act =
                trim TrimOption.all ((=) 0) input
            let exp =
                Some [|
                    [| 1 |]
                |]
            Assert.Equal("", exp, act)

        testCase "1x2" <| fun () ->
            let input =
                [|
                    [| 0; 1; 0 |]
                    [| 0; 2; 0 |]
                |]
            let act =
                trim TrimOption.all ((=) 0) input
            let exp =
                Some [|
                    [| 1 |]
                    [| 2 |]
                |]
            Assert.Equal("", exp, act)

        testCase "2x1" <| fun () ->
            let input =
                [|
                    [| 0; 0 |]
                    [| 1; 2 |]
                    [| 0; 0 |]
                |]
            let act =
                trim TrimOption.all ((=) 0) input
            let exp =
                Some [|
                    [| 1; 2 |]
                |]
            Assert.Equal("", exp, act)

        testCase "2x3" <| fun () ->
            let input =
                [|
                    [| 0; 0; 0; 0 |]
                    [| 0; 1; 2; 0 |]
                    [| 0; 3; 4; 0 |]
                    [| 0; 5; 6; 0 |]
                    [| 0; 0; 0; 0 |]
                    [| 0; 0; 0; 0 |]
                |]
            let act =
                trim TrimOption.all ((=) 0) input
            let exp =
                Some [|
                    [| 1; 2 |]
                    [| 3; 4 |]
                    [| 5; 6 |]
                |]
            Assert.Equal("", exp, act)

        testCase "3x2" <| fun () ->
            let input =
                [|
                    [| 0; 0; 0; 0 |]
                    [| 0; 1; 2; 3 |]
                    [| 0; 4; 5; 6 |]
                    [| 0; 0; 0; 0 |]
                |]
            let act =
                trim TrimOption.all ((=) 0) input
            let exp =
                Some [|
                    [| 1; 2; 3 |]
                    [| 4; 5; 6 |]
                |]
            Assert.Equal("", exp, act)

        testCase "3x3" <| fun () ->
            let input =
                [|
                    [| 0; 0; 0; 0; 0 |]
                    [| 0; 0; 1; 0; 0 |]
                    [| 0; 2; 3; 4; 0 |]
                    [| 0; 0; 5; 0; 0 |]
                    [| 0; 0; 0; 0; 0 |]
                    [| 0; 0; 0; 0; 0 |]
                |]
            let act =
                trim TrimOption.all ((=) 0) input
            let exp =
                Some [|
                    [| 0; 1; 0 |]
                    [| 2; 3; 4 |]
                    [| 0; 5; 0 |]
                |]
            Assert.Equal("", exp, act)

        testCase "2x2 left trim" <| fun () ->
            let input =
                [|
                    [| 0; 0; 0; 0 |]
                    [| 0; 1; 0; 0 |]
                    [| 0; 0; 2; 0 |]
                    [| 0; 0; 0; 0 |]
                |]
            let act =
                trim TrimOption.Left ((=) 0) input
            let exp =
                Some [|
                    [| 0; 0; 0 |]
                    [| 1; 0; 0 |]
                    [| 0; 2; 0 |]
                    [| 0; 0; 0 |]
                |]
            Assert.Equal("", exp, act)

        testCase "2x2 top trim" <| fun () ->
            let input =
                [|
                    [| 0; 0; 0; 0 |]
                    [| 0; 1; 0; 0 |]
                    [| 0; 0; 2; 0 |]
                    [| 0; 0; 0; 0 |]
                |]
            let act =
                trim TrimOption.Top ((=) 0) input
            let exp =
                Some [|
                    [| 0; 1; 0; 0 |]
                    [| 0; 0; 2; 0 |]
                    [| 0; 0; 0; 0 |]
                |]
            Assert.Equal("", exp, act)

        testCase "2x2 right trim" <| fun () ->
            let input =
                [|
                    [| 0; 0; 0; 0 |]
                    [| 0; 1; 0; 0 |]
                    [| 0; 0; 2; 0 |]
                    [| 0; 0; 0; 0 |]
                |]
            let act =
                trim TrimOption.Right ((=) 0) input
            let exp =
                Some [|
                    [| 0; 0; 0 |]
                    [| 0; 1; 0 |]
                    [| 0; 0; 2 |]
                    [| 0; 0; 0 |]
                |]
            Assert.Equal("", exp, act)

        testCase "2x2 bottom trim" <| fun () ->
            let input =
                [|
                    [| 0; 0; 0; 0 |]
                    [| 0; 1; 0; 0 |]
                    [| 0; 0; 2; 0 |]
                    [| 0; 0; 0; 0 |]
                |]
            let act =
                trim TrimOption.Bottom ((=) 0) input
            let exp =
                Some [|
                    [| 0; 0; 0; 0 |]
                    [| 0; 1; 0; 0 |]
                    [| 0; 0; 2; 0 |]
                |]
            Assert.Equal("", exp, act)

        testCase "2x2 left and top trim" <| fun () ->
            let input =
                [|
                    [| 0; 0; 0; 0 |]
                    [| 0; 1; 0; 0 |]
                    [| 0; 0; 2; 0 |]
                    [| 0; 0; 0; 0 |]
                |]
            let act =
                trim (TrimOption.Left ||| TrimOption.Top) ((=) 0) input
            let exp =
                Some [|
                    [| 1; 0; 0 |]
                    [| 0; 2; 0 |]
                    [| 0; 0; 0 |]
                |]
            Assert.Equal("", exp, act)

        testCase "2x2 right and bottom trim" <| fun () ->
            let input =
                [|
                    [| 0; 0; 0; 0 |]
                    [| 0; 1; 0; 0 |]
                    [| 0; 0; 2; 0 |]
                    [| 0; 0; 0; 0 |]
                |]
            let act =
                trim (TrimOption.Right ||| TrimOption.Bottom) ((=) 0) input
            let exp =
                Some [|
                    [| 0; 0; 0 |]
                    [| 0; 1; 0 |]
                    [| 0; 0; 2 |]
                |]
            Assert.Equal("", exp, act)
    ]
