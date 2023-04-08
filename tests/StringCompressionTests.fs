module StringCompressionTests
open Fuchu
open FsharpMyExtension.StringCompression

[<Tests>]
let stringCompressionTests =
    testList "stringCompressionTests" [
        testCase "base" <| fun () ->
            let input = "\"{\"Id\":\"SimpleQuizUI\",\"CID\":0,\"D\":{\"OwnerId\":796931597898088448,\"QuizId\":\"capitals\",\"QuestionId\":\"1\"}}\""
            let compressedString = compress input
            let act = decompress compressedString
            Assert.Equal("", input, act)
    ]
