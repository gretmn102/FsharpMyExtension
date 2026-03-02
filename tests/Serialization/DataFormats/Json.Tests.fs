module FsharpMyExtension.Serialization.DataFormats.Tests
open Fuchu

type T = { Id: string; Y: string }

[<Tests>]
let bsonSerializerTests =
    testList "bsonSerializerTests" [
        testCase "base" <| fun () ->
            let input = { Id = "SimpleQuizUI"; Y = "2000" }
            let act: T = Bson.deserializeBytes (Bson.serialize input)
            Assert.Equal("", input, act)
    ]


module SerializeOptionTests =
    type TypeWithOption = {
        Opt: int option
    }

    type TypeWithOptions = TypeWithOption list

    let value: TypeWithOptions = [
        { Opt = None }
        { Opt = Some 10 }
    ]

    let jsonExp =
        String.concat System.Environment.NewLine [
            "["
            "  {"
            "    \"Opt\": null"
            "  },"
            "  {"
            "    \"Opt\": 10"
            "  }"
            "]"
        ]

    [<Tests>]
    let ``SerializeOption.ser`` =
        testList "SerializeOption.ser" [
            testCase "base" <| fun () ->
                let act = FSharpJsonType.SerializeOption.ser value

                Assert.Equal("", jsonExp, act)
        ]

    [<Tests>]
    let ``SerializeOption.des`` =
        testList "SerializeOption.des" [
            testCase "base" <| fun () ->
                let exp = value
                let act: TypeWithOptions =
                    FSharpJsonType.SerializeOption.des jsonExp

                Assert.Equal("", exp, act)
        ]
