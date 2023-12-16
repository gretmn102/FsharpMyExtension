module FsharpMyExtension.Collections.LazyTree.Tests
open Fuchu

open FsharpMyExtension.Collections

[<Tests>]
let VisualizeTest =
    testList "VisualizeTest" [
        testCase "base case" <| fun () ->
            let dummy =
                LT
                    ("1",
                        seq {
                            yield LT ("2",seq { yield LT ("3",Seq.empty); yield LT ("4",Seq.empty)});
                            yield LT ("3",seq { yield LT ("4",Seq.empty)})
                        }
                    )
            let exp =
                [
                    "1"
                    "├─2"
                    "│ ├─3"
                    "│ └─4"
                    "└─3"
                    "  └─4"
                ] |> String.concat "\n"
            let act = visualize (sprintf "%s") dummy
            Assert.Equal("", exp, act)
    ]
