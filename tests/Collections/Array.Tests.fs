module FsharpMyExtension.Collections.Array.Tests
open Fuchu

open FsharpMyExtension.Primitives

[<Tests>]
let splitTests =
    let test sep input =
        testCase input <| fun () ->
            let exp = String.split sep input
            let act =
                split (sep.ToCharArray()) (input.ToCharArray())
                |> Array.map System.String.Concat
            Assert.Equal("", exp, act)

    testList "splitTests" [
        test "++" "item"
        test "++" "++"
        test "++" "sword++shield"
        test "<>" "sword<> arrow<>bow<>knife <>health potion<><>nothing"
    ]

[<Tests>]
let mapStartMidEndTests =
    testList "splitTests" [
        testCase "one" <| fun () ->
            let act =
                [|1..10|]
                |> mapStartMidEnd
                    (fun i -> i - 1)
                    (fun i -> i + 1)
                    (fun i -> i - 2)
            let exp = [|0; 3; 4; 5; 6; 7; 8; 9; 10; 8|]
            Assert.Equal("", exp, act)
    ]

[<Tests>]
let removeAtTests =
    testList "removeAtTests" [
        yield! [
                [||], (0, [|0|])
                [|2; 3|], (0, [|1..3|])
                [|1; 3|], (1, [|1..3|])
                [|1; 2|], (2, [|1..3|])
                [|1; 2; 3; 4; 5|], (0, [|0..5|])
                [|0; 1; 3; 4; 5|], (2, [|0..5|])
                [|0; 1; 2; 3; 4|], (5, [|0..5|])
            ]
            |> List.map (fun (exp, (i, xs)) ->
                testCase (sprintf "(%d, %A)" i xs) <| fun () ->
                    Assert.Equal("", exp, removeAt i xs)
            )
    ]

[<Tests>]
let generateRandomNumbersBySumTest =
    let f length dice sum =
        List.init 20 (fun _ -> generateRandomNumbersBySum length dice sum)
        |> List.forall (fun xs -> xs |> Array.sum = sum)

    Fuchu.Tests.testList "generateRandomNumbersBySumTest" [
        testCase "A simple test" (fun _ ->
            Assert.Equal("", true, f 6 (3, 6) 20)
        )
        testCase "A simple test2" (fun _ ->
            Assert.Equal("", true, f 3 (1, 4) 10)
        )
        testCase "m1 > m2" (fun _ ->
            Assert.Raise("", typeof<exn>,
                fun _ -> generateRandomNumbersBySum 4 (3, 2) 10 |> ignore)
        )
        testCase "sum < m1 * n" (fun _ ->
            Assert.Raise("", typeof<exn>,
                fun _ -> generateRandomNumbersBySum 4 (1, 2) 3 |> ignore)
        )
        testCase "sum > m2 * n" (fun _ ->
            Assert.Raise("", typeof<exn>,
                fun _ -> generateRandomNumbersBySum 4 (1, 2) 10 |> ignore)
        )
    ]

[<Tests>]
let swapTests =
    testList "swapTests" [
        testCase "Array.swap_1" <| fun () ->
            let xs = [|'a'..'e'|]
            let sourceId, targetId = 1, 4
            let act = swap sourceId targetId xs
            let exp = [|'a'; 'c'; 'd'; 'e'; 'b'|]
            Assert.Equal("", exp, act)
        testCase "Array.swap_2" <| fun () ->
            let xs = [|'a'..'e'|]
            let sourceId, targetId = 4, 1
            let act = swap sourceId targetId xs
            let exp = [|'a'; 'e'; 'b'; 'c'; 'd'|]
            Assert.Equal("", exp, act)
        testCase "Array.swap_3" <| fun () ->
            let xs = [|'a'..'e'|]
            let sourceId, targetId = 1, 1
            let act = swap sourceId targetId xs
            let exp = xs
            Assert.Equal("", exp, act)
    ]

[<Tests>]
let genericBinarySearchTests =
    let binarySearch target xs =
        let compare (xs: int []) (target: int) (idx: int) =
            xs.[idx].CompareTo target
        let xs = xs |> Array.sort
        genericBinarySearch (compare xs target) xs.Length

    let createTest (target, xs) exp =
        testCase (sprintf "(%d, %A)" target xs) <| fun () ->
            let act = binarySearch target xs

            Assert.Equal("", exp, act)

    let range x =
        GenericBinarySearchResult.Range x

    let exactly x =
        GenericBinarySearchResult.Exactly x

    testList "genericBinarySearchTests" [
        createTest (-1, [|0|]) (range (-1, 0))
        createTest (0, [|0|]) (exactly 0)
        createTest (1, [|0|]) (range (0, 1))

        createTest (-1, [|0; 1|]) (range (-1, 0))
        createTest (0, [|0; 1|]) (exactly 0)
        createTest (1, [|0; 1|]) (exactly 1)
        createTest (2, [|0; 1|]) (range (1, 2))

        createTest (1, [|0; 2|]) (range (0, 1))

        createTest (-1, [|0; 2; 4|]) (range (-1, 0))
        createTest (0, [|0; 2; 4|]) (exactly 0)
        createTest (1, [|0; 2; 4|]) (range (0, 1))
        createTest (2, [|0; 2; 4|]) (exactly 1)
        createTest (3, [|0; 2; 4|]) (range (1, 2))
        createTest (4, [|0; 2; 4|]) (exactly 2)
        createTest (5, [|0; 2; 4|]) (range (2, 3))

        createTest (-1, [|0; 2; 4; 6|]) (range (-1, 0))
        createTest (7, [|0; 2; 4; 6|]) (range (3, 4))

        createTest (-1, [|0; 2; 4; 6; 8|]) (range (-1, 0))
        createTest (9, [|0; 2; 4; 6; 8|]) (range (4, 5))
    ]

[<Tests>]
let binarySearchTests =
    let binarySearch target xs =
        let compare (x: int) =
            x.CompareTo target

        let xs = xs |> Array.sort

        binarySearch compare xs

    let createTest (target, xs) exp =
        testCase (sprintf "(%d, %A)" target xs) <| fun () ->
            let act = binarySearch target xs

            Assert.Equal("", exp, act)

    testList "binarySearchTests" [
        createTest (System.Int32.MinValue, [| 0..5 |]) -1
        createTest (System.Int32.MaxValue, [| 0..5 |]) 5
        createTest (3, [| 0..5 |]) 3
    ]
