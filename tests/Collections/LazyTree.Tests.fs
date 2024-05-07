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

[<Tests>]
let packTest =
    testList "packTest" [
        testCase "base case" <| fun () ->
        let exp =
            [[1; 2; 3]; [1; 2; 4]; [1; 2; 5]; [1; 2; 6]; [1; 3; 4]; [1; 3; 5]; [1; 3; 6];
             [1; 4; 5]; [1; 4; 6]; [1; 5; 6]; [2; 3; 4]; [2; 3; 5]; [2; 3; 6]; [2; 4; 5];
             [2; 4; 6]; [2; 5; 6]; [3; 4; 5]; [3; 4; 6]; [3; 5; 6]; [4; 5; 6]]
        let act = pack exp |> unpack |> List.ofSeq
        Assert.Equal("", exp, act)
    ]
