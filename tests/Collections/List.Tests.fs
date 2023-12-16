module FsharpMyExtension.Collections.List.Tests
open Fuchu
open FsharpMyExtension.Commons

[<Tests>]
let groupBySeqTest =
    testList "groupBySeqTest" [
        testCase "base" <| fun () ->
            Assert.Equal("", [], groupBySeq (=) [])
        testCase "one" <| fun () ->
            let xs = [1]
            Assert.Equal("", [xs], groupBySeq (=) xs)
        testCase "many" <| fun () ->
            let xs = [[1;1]; [2]; [1; 1]; [3; 3;3]; [4; 4]; [5;5;5]]
            Assert.Equal("", xs, groupBySeq (=) (List.concat xs))
        testCase "many with function" <| fun () ->
            let xs = [[1;1]; [2]; [1; 1]; [3; 3; 3]; [4; 4]; [5;5;5]]
            xs |> List.mapFold (fun st -> List.mapFold (fun st x -> (st, x), st + 1) st) 0
            |> fst |> fun xs ->
            Assert.Equal("", xs, groupBySeq (fun x y -> snd x = snd y) (List.concat xs))
        testCase "many2 with function" <| fun () ->
            let xs = [ [1,1; 2,1;]; [3,2; 4,2] ]
            Assert.Equal("", xs, groupBySeq (fun x y -> snd x = snd y) (List.concat xs))
    ]
[<Tests>]
let circleTest =
    let circle count =
        circle >> Seq.take count >> List.ofSeq
    testList "circleTest" [
        testCase "base case" <| fun () ->
            let f () = circle 10 [] |> ignore
            Assert.Raise("[] -> exception", typeof<System.ArgumentException>, f )
        testCase "one case" <| fun () ->
            let x, count = 1, 10
            let xs = [x]
            Assert.Equal(sprintf "xs = %A; count = %d" xs count,
                List.replicate count x, circle count xs )
        testCase "many case" <| fun () ->
            let xs, count = [1..3], 7
            Assert.Equal(sprintf "xs = %A; count = %d" xs count,
                [1; 2; 3; 1; 2; 3; 1], circle count xs)
    ]

[<Tests>]
let numerateTest =
    let test lab xs =
        testCase lab <| fun () ->
            let xs = numerate None xs |> List.ofSeq
            let expected, act = List.sortBy fst xs, List.sortBy snd xs
            Assert.Equal(sprintf "%s error" lab, expected, act)
    testList "numerateTest" [
        testCase "base case" <| fun () ->
            Assert.Equal("", [], numerate None [])
        test "digits" [0..9]
        test "tens" [0..99]
        test "hundreds" [0..999]
        testCase "random case" <| fun () ->
            let xs = [1;3;5;1;3;30;20;10;30;40;50;10;323]
            let x =
                xs |> numerate None |> List.sortBy snd
                |> List.zip xs
                |> List.tryFind (fun (x, y) -> x <> fst y)
            Assert.Equal(sprintf "%A" xs, None, x)
        testCase "print function (_ -> \".\")" <| fun () ->
            let xs = [0..30]
            let x =
                numerate (Some (fun _ -> ".")) xs
                |> List.tryFind (snd >> fun x -> x.EndsWith "." |> not)
            Assert.Equal(sprintf "arg [0..30], must be [0,\"00.\"; 0,\"01.\"...]", None, x)
    ]

[<Tests>]
let concatSepTest =
    let inline gen (sep: char) xs =
        let xs = List.ofSeq xs
        let exp = String.concat (sep.ToString()) (List.map string xs)
        let act = concatSep sep xs |> System.String.Concat
        testCase "" <| fun _ -> Assert.Equal("", exp, act)
    seq {
        yield gen '+' "abcde"
        yield gen '+' "abcd"
        yield gen '+' "abc"
        yield gen '+' "ab"
        yield gen '+' "a"
        yield gen '+' ""
    }
    |> testList "List.concatSepTest"

[<Tests>]
let chooseFoldTest =
    let xs = [1..10]
    let stExp = List.sum xs
    let xsExp = List.filter isEven xs
    let act =
        xs
        |> chooseFold (fun st x ->
            let x' = if isEven x then Some x else None
            x', st + x
            ) 0
    testCase "chooseFoldTest base" <| fun _ ->
        Assert.Equal("", (xsExp, stExp), act)

[<Tests>]
let sepByTests =
    testList "sepByTests" [
        testCase "empty" <| fun _ ->
            Assert.Equal("", [], sepBy "+" [])
        testCase "1" <| fun _ ->
            Assert.Equal("", ["1"], sepBy "+" ["1"])
        testCase "2" <| fun _ ->
            Assert.Equal("", ["1"; "+"; "2"], sepBy "+" ["1"; "2"])
        testCase "many" <| fun _ ->
            let values =
                List.init 5 (id >> string)
            let exp =
                ["0"; "+"; "1"; "+"; "2"; "+"; "3"; "+"; "4"]
            Assert.Equal("", exp, sepBy "+" values)
    ]
