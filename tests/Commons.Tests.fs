module FsharpMyExtension.Commons.Tests
open Fuchu
open FParsec

[<Tests>]
let For'Test =
    testList "For'Test" [
        testCase "base case" <| fun () ->
            let st = -1
            Assert.Equal("", st, for' 1 0 (fun _ _ -> 2) st)
        testCase "fact" <| fun () ->
            Assert.Equal("", List.reduce (*) [1..12], for' 1 12 (*) 1)
    ]

type Item =
    {
        Name: string
        Cost: int
    }
    static member Deserialize str =
        let p =
            pipe2
                (manySatisfy ((<>) '\n') .>> newline)
                pint32
                (fun name cost ->
                    {
                        Name = name
                        Cost = cost
                    }
                )

        match run p str with
        | Success(res, _, _) -> Result.Ok res
        | Failure(errMsg, _, _) -> Result.Error errMsg

    static member Empty : Item =
        {
            Name = ""
            Cost = 0
        }

[<Tests>]
let deserializeTests =
    testList "deserializeTests" [
        testCase "base" <| fun () ->
            let exp =
                Result.Ok {
                    Name = "Sword"
                    Cost = 300
                }
            let act =
                ((deserialize "Sword\n300") : Result<Item, string>)

            Assert.Equal("", exp, act)
    ]

[<Tests>]
let emptyTests =
    testList "emptyTests" [
        testCase "base" <| fun () ->
            let exp =
                {
                    Name = ""
                    Cost = 0
                }
            let act =
                empty

            Assert.Equal("", exp, act)
    ]
