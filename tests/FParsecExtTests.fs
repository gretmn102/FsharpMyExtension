module FParsecExtTests
open Fuchu
open FsharpMyExtension
open FsharpMyExtension.Containers.Either
open FsharpMyExtension.FParsecExt

open FParsec

[<Tests>]
let pbigintTests =
    testList "pbigintTests" [
        testCase "testCase1" (fun _ ->
            let exp = 1234I
            let act = runEither pbigint (string exp)

            Assert.Equal("", act, Right exp)
        )
        testCase "testCase2" (fun _ ->
            let exp = 12345678901234567890I
            let act = runEither pbigint (string exp)

            Assert.Equal("", act, Right exp)
        )
    ]

[<Tests>]
let runParserOnSubstringStartTests =
    testList "runParserOnSubstringStartTests" [
        testCase "base" <| fun () ->
            let str = "ab\nc"

            let exp = Result.Ok "c"

            let act =
                runParserOnSubstringStart (pstring "ab" .>> newline) 0 str
                |> ParserResult.toResult
                |> Result.bind (fun (res, _, pos) ->
                    runParserOnSubstringStart (pstring "c") (int pos.Index) str
                    |> ParserResult.toResult
                )
                |> Result.map (fun (res, _, _) -> res)

            Assert.Equal("", exp, act)
    ]
