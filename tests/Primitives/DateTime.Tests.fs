module FsharpMyExtension.Primitives.DateTime.Tests
open Fuchu

[<Tests>]
let daysStepTest =
    testList "daysStepTest" [
        testCase "base case min" <| fun () ->
            let act = List.ofSeq (daysStep -1 System.DateTime.MinValue)
            Assert.Equal("", [], act)
        testCase "base case max" <| fun () ->
            let act = List.ofSeq (daysStep 1 System.DateTime.MaxValue)
            Assert.Equal("", [], act)
        testCase "one case min" <| fun () ->
            let d = System.DateTime.MinValue
            let exp = [d]
            let x = d.AddDays 1.
            let act = List.ofSeq (daysStep -1 x)
            Assert.Equal("", exp, act)
        testCase "one case max" <| fun () ->
            let d = System.DateTime.MaxValue
            let exp = [d]
            let x = d.AddDays -1.
            let act = List.ofSeq (daysStep 1 x)
            Assert.Equal("", exp, act)
    ]
