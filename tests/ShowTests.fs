module Tests.Show
open Fuchu
open FsharpMyExtension.ShowList

[<Tests>]
let joinTest =
    testList "joinTest" [
        testCase "base case" <| fun () ->
            let exp = "a, b, c"
            let act =
                [
                    empty
                    empty
                    showString "a"
                    showString "b"
                    empty
                    empty
                    showString "c"
                    empty
                    empty
                ]
                |> joins (showString ", ")
                |> show

            Assert.Equal("", exp, act)
    ]

[<Tests>]
let joinEmptyTest =
    testList "joinEmptyTest" [
        testCase "base case" <| fun () ->
            let exp = ", , a, b, , , c, , "

            let act =
                [
                    empty
                    empty
                    showString "a"
                    showString "b"
                    empty
                    empty
                    showString "c"
                    empty
                    empty
                ]
                |> joinEmpty ", "
                |> show

            Assert.Equal("", exp, act)
    ]
