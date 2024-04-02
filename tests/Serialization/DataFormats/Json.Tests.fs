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
