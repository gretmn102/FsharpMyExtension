module FsharpMyExtension.ResultExt.Tests
open FsharpMyExtension.ResultExt
open Fuchu

[<Tests>]
let Tests =
    testList "Tests" [
        testCase "Ok _" <| fun () ->
            let act =
                Result.builder {
                    let! x = Ok 2
                    let! y = Ok 1
                    if x < y then
                        return! Error "x < y"
                    else
                        return x + y
                }

            let exp = Ok 3

            Assert.Equal("", exp, act)

        testCase "Error _" <| fun () ->
            let act =
                Result.builder {
                    let! x = Ok 0
                    let! y = Ok 1
                    if x < y then
                        return! Error "x < y"
                    else
                        return x + y
                }

            let exp = Error "x < y"

            Assert.Equal("", exp, act)
    ]
