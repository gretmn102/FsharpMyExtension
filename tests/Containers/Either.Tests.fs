module FsharpMyExtension.Containers.Either.Tests
open Fuchu

open FsharpMyExtension.Containers

[<Tests>]
let seqEitherPseudoTest =
    testList "seqEitherPseudoTest" [
        testCase "BaseCase" <| fun () ->
            let act =
                Right Seq.empty |> seqEitherPseudo |> List.ofSeq
            Assert.Equal("", [], act)
        testCase "Case2" <| fun () ->
            let act =
                Left "0" |> seqEitherPseudo |> List.ofSeq
            Assert.Equal("", [Left "0"], act)
        testCase "Case3" <| fun () ->
            let act =
                Right (seq[ Left "0"; Right 1 ]) |> seqEitherPseudo |> List.ofSeq
            Assert.Equal("", [Left "0"; Right 1], act)
    ]

[<Tests>]
let collectTest =
    testList "collectTest" [
        testCase "BaseCase" <| fun () ->
            let act =
                Right 0 |> collect (Seq.singleton << Right) |> List.ofSeq
            Assert.Equal("", [Right 0], act)
        testCase "Case2" <| fun () ->
            let act =
                Left "error" |> collect (Seq.singleton << Right) |> List.ofSeq
            Assert.Equal("", [Left "error"], act)
        testCase "Case3" <| fun () ->
            let act =
                Right ()
                |> collect (fun _ -> seq [ Right 0; Left "left"; Right 1 ])
                |> List.ofSeq
            Assert.Equal("", [Right 0; Left "left"; Right 1], act)
    ]

[<Tests>]
let seqOptTest =
    testList "seqOptTest" [
        testCase "BaseCase1" <| fun () ->
            let act =
                seqOpt (Left "")
            Assert.Equal("", Some(Left ""), act)
        testCase "BaseCase2" <| fun () ->
            let act =
                seqOpt (Right (Some ""))
            Assert.Equal("", Some(Right ""), act)
        testCase "BaseCase3" <| fun () ->
            let act =
                seqOpt (Right None)
            Assert.Equal("", None, act)
    ]
