module Tests.Show
open Fuchu
open FsharpMyExtension.ShowList

type Item =
    {
        Name: string
        Cost: int
    }

    interface IShow with
        member this.Shows () =
            showString this.Name << nl
            << showByToString this.Cost

[<Tests>]
let showsTests =
    testList "showsTests" [
        testCase "base" <| fun () ->
            let exp =
                sprintf "Sword%s100" System.Environment.NewLine

            let act =
                { Name = "Sword"; Cost = 100 }
                |> shows
                |> show

            Assert.Equal("", exp, act)
    ]

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
