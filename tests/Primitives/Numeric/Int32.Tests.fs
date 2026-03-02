module FsharpMyExtension.Primitives.Numeric.Int32.Tests
open Fuchu

open Helpers

[<Tests>]
let getLengthTests =
    let createTest n =
        testCase (sprintf "%d" n) <| fun () ->
            let exp =
                (n.ToString()).Length
            let act =
                getLength n

            Assert.Equal("", exp, act)

    testList "getLengthTests" [
        createTest 1
        createTest 12
        createTest 123
        createTest 1234
        createTest 12345
        createTest 123456
        createTest 1234567
        createTest 12345678
        createTest 123456789
        createTest 9
        createTest 98
        createTest 987
        createTest 9876
        createTest 98765
        createTest 987654
        createTest 9876543
        createTest 98765432
        createTest 987654321
    ]

[<Tests>]
let digitsSplitAndJoinTests =
    let genRandomList () =
        let r = System.Random()
        let length = r.Next(1, getLength System.Int32.MaxValue)
        List.init length (fun i ->
            if i > 0 then r.Next(0, 10)
            else r.Next(1, 10)
        )

    let test input =
        testCase (sprintf "%A" input) <| fun () ->
            let exp = input
            let act =
                toDigits (ofDigits input)

            Assert.Equal("", exp, act)

    testList "digitsSplitAndJoinTests" [
        test [3; 1; 4; 5; 6; 1]
        test [1; 6; 4; 6; 2; 4; 4; 9]
        test [8; 9; 4; 0; 9; 9; 6]
    ]

[<Tests>]
let ``Primitives.Numeric.Int32.zeroPad`` =
    testList "Primitives.Numeric.Int32.zeroPad" [
        testCase "0 100 = 100" <| fun () ->
            Expect.equal (zeroPad 0 100) "100" ""
        testCase "1 1 = 1" <| fun () ->
            Expect.equal (zeroPad 1 1) "1" ""
        testCase "2 9 = 09" <| fun () ->
            Expect.equal (zeroPad 2 9) "09" ""
    ]
