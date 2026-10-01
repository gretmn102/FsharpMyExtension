module FsharpMyExtension.Graphics.Size.Tests
open Fuchu

open FsharpMyExtension.Grahics

open Helpers

[<Tests>]
let ``Graphics.Size.imageSize`` =
    testList "Graphics.Size.imageSize" [
        testCase "base" <| fun () ->
            Expect.equal
                (
                    let imageSize = Size.create 4 3
                    let scale = Size.fitScale (Size.create 3 4) imageSize
                    imageSize * scale
                )
                (Size.create 3 2)
                ""
    ]
