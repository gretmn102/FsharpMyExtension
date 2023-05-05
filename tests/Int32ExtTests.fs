module Int32ExtTests
open FsharpMyExtension
open Fuchu

[<Tests>]
let getLengthTests =
    let createTest n =
        testCase (sprintf "%d" n) <| fun () ->
            let exp =
                (n.ToString()).Length
            let act =
                Int32.getLength n

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
