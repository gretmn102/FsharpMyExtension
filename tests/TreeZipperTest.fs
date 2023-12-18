module FsharpMyExtension.Collections.TreeZipper.Tests
open Fuchu

open FsharpMyExtension.Collections

[<Tests>]
let treeZipperTests =
    testList "TreeZipperTests" [
        let sample =
            Node("a", [
                Node("b", [])
                Node("c", [
                    Node("d", [
                        Node("e", [])
                    ])
                    Node("f", [])
                ])
                Node("g", [])
            ])

        let root = ofTree sample

        testCase "next, down, up" <| fun () ->
            let exp =
                Some
                  [("a",
                    Some
                      { Index = 2
                        Left = []
                        Current =
                            Node ("c1", [Node ("f", []); Node ("d2", [Node ("e", [])])])
                        Right = [Node ("g", []); Node ("b", [])] })]

            let act =
                next root
                |> Option.bind down
                |> Option.map (update (fun x -> x + "1"))
                |> Option.bind down
                |> Option.map (update (fun x -> x + "2"))
                |> Option.map up
                |> Option.map up

            Assert.Equal("", exp, act)

        testCase "append, down" <| fun () ->
            let appendAndDown x = append x >> down >> Option.get

            let act =
                ["root", None]
                |> appendAndDown "one"
                |> append "two"
                |> append "three"
                |> up

            let exp =
                [("root",
                  Some { Index = 0
                         Left = []
                         Current = Node ("one", [Node ("two", []); Node ("three", [])])
                         Right = [] })]

            Assert.Equal("", exp, act)
    ]
