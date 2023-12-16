module TreeZipperTest
open Fuchu

open FsharpMyExtension
open FsharpMyExtension.Collections
open FsharpMyExtension.Collections.TreeZipper

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

        let root = TreeZipper.ofTree sample

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
                TreeZipper.next root
                |> Option.bind TreeZipper.down
                |> Option.map (TreeZipper.update (fun x -> x + "1"))
                |> Option.bind TreeZipper.down
                |> Option.map (TreeZipper.update (fun x -> x + "2"))
                |> Option.map TreeZipper.up
                |> Option.map TreeZipper.up

            Assert.Equal("", exp, act)

        testCase "append, down" <| fun () ->
            let appendAndDown x = TreeZipper.append x >> TreeZipper.down >> Option.get

            let act =
                [("root", None)]
                |> appendAndDown "one"
                |> TreeZipper.append "two"
                |> TreeZipper.append "three"
                |> TreeZipper.up

            let exp =
                [("root",
                  Some { Index = 0
                         Left = []
                         Current = Node ("one", [Node ("two", []); Node ("three", [])])
                         Right = [] })]

            Assert.Equal("", exp, act)
    ]
