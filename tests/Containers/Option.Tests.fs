module FsharpMyExtension.Containers.Option.Tests
open Fuchu

module Seq =
    let seqOpt = FsharpMyExtension.Collections.Seq.seqOpt

    [<Tests>]
    let seqOptTest =
        testList "seqOptTest" [
            testCase "base case" <| fun () ->
                Assert.Equal("", Some [], seqOpt Seq.empty)
            testCase "many Some case" <| fun () ->
                let xs = [0..9]
                Assert.Equal("", Some xs, List.map Some xs |> seqOpt)
            testCase "has None case" <| fun () ->
                let xs = seq{ yield Some 1; yield None; yield Some 3 }
                Assert.Equal("", None, seqOpt xs)
            testCase "lazyness test" <| fun () ->
                let counter = ref 0
                let xs = seq{
                    yield Some 1; counter.Value <- counter.Value + 1;
                    yield None; counter.Value <- counter.Value + 1;
                    yield Some 3; counter.Value <- counter.Value + 1; }
                seqOpt xs |> ignore
                Assert.Equal("", 1, counter.Value)
        ]
