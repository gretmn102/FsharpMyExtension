module FsharpMyExtension.Containers.Tests
open Fuchu

let travBaseTests testListName fn =
    testList testListName [
        testCase "single Left" <| fun () ->
            let x = "some"
            Assert.Equal("", Left x, fn [Left x])
        testCase "single Right" <| fun () ->
            let x = 0
            Assert.Equal("", Right [x], fn [Right x])
        testCase "seq with Left" <| fun () ->
            let x = "error"
            Assert.Equal("", Left x, fn [Right 0; Left x; Right 1])
        testCase "seq only with Right" <| fun () ->
            Assert.Equal("", Right [0..9], List.init 10 Right |> fn)
    ]

module ListTests =
    [<Tests>]
    let listTrav = travBaseTests "List.seqEitherBase" List.seqEither

    [<Tests>]
    let partitionEither =
        let part = List.partitionEithers
        testList "partitionEither" [
            testCase "empty" <| fun () ->
                Assert.Equal("", ([],[]), part [])
            testCase "mix" <| fun () ->
                let xs = [Left "1"; Right 2; Left "3"; Right 4]
                let ys = part xs
                Assert.Equal("", (["1"; "3";], [2; 4]), part xs)
            testCase "mix2" <| fun () ->
                let xs = [Right "1"; Left 2; Right "3"; Left 4]
                Assert.Equal("", ([2; 4], ["1"; "3";]), part xs)
            // testCase "seq with Left" <| fun () ->
            //     let x = "error"
            //     Assert.Equal("", Left x, fn [Right 0; Left x; Right 1])
            // testCase "seq only with Right" <| fun () ->
            //     Assert.Equal("", Right [0..9], List.init 10 Right |> fn)
        ]

module SeqTests =
    [<Tests>]
    let seqTrav = travBaseTests "Seq.seqEitherBase" Seq.seqEither

    [<Tests>]
    let testLaziness =
        testCase "lazyness test" <| fun () ->
            let counter = ref 0
            seq{ yield Right 1; incr counter;
                 yield Left -1; incr counter;
                 yield Right 3; incr counter; } |> Seq.seqEither |> ignore
            Assert.Equal("", 1, !counter)

module OptionTests =
    [<Tests>]
    let isAnyOfTest =
        testList "Option.ofEither" [
            testCase "left" <| fun () ->
                Assert.Equal("Left _ = None", None, Option.ofEither (Left ()))
            testCase "right" <| fun () ->
                let x = 1
                Assert.Equal("Right x = Some x", Some x, Option.ofEither (Right x))
        ]
