module Tests.Show
open Fuchu
open FsharpMyExtension.ShowList

[<Tests>]
let joinTest =
    testList "joinTest" [
        testCase "base case" <| fun () ->
            let exp = "Abram, Lyouis, Loid"
            let xs = [empty;empty;showString "Abram";showString "Lyouis";empty;empty;showString "Loid";empty;empty]
            let act =
                joins (showString ", ") xs
                |> show
                |> System.String.Concat
            Assert.Equal("", exp, act)
    ]

[<Tests>]
let joinEmptyTest =
    testList "joinTest" [
        testCase "base case" <| fun () ->
            let exp = ", , Abram, Lyouis, , , Loid, , "
            let xs = [empty;empty;showString "Abram";showString "Lyouis";empty;empty;showString "Loid";empty;empty]
            let act =
                joinEmpty ", " xs
                |> show
                |> System.String.Concat
            Assert.Equal("", exp, act)
    ]
