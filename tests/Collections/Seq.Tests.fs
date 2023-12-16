module FsharpMyExtension.Collections.Seq.Tests
open Fuchu

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
    |> testList "concatSepTest"
    // TODO: test on lazy
    // let xs =
    //     seq{
    //         printfn "a"; yield 'a';
    //         printfn "b"; yield 'b';
    //         printfn "b"; yield 'c';
    //         printfn "b"; yield 'd';
    //         failwith ""
    //         printfn "e"; yield 'e';
    //     }
    // concatSepSeqSeq '+' xs |> List.ofSeq

[<Tests>]
let concatSepTest2 =
    testList "concatSepTest" [
        testCase "testCase1" (fun _ ->
            let exp = Array.ofSeq "a"
            let act = Array.ofSeq (concatSep '+' "a")
            Assert.Equal("", exp, act)
        )
        testCase "testCase2" (fun _ ->
            let exp = Array.ofSeq "a+b"
            let act = Array.ofSeq (concatSep '+' "ab")
            Assert.Equal("", exp, act)
        )
        testCase "testCase3" (fun _ ->
            let exp = Array.ofSeq "a+b+c"
            let act = Array.ofSeq (concatSep '+' "abc")
            Assert.Equal("", exp, act)
        )
        testCase "test consistency" (fun _ ->
            let xs = concatSep '+' "abcdef"
            Seq.head xs |> ignore
            let exp = "a+b+c+d+e+f".ToCharArray()
            let act = Array.ofSeq xs
            Assert.Equal("", exp, act)
        )
    ]

[<Tests>]
let generateRandomSequenceTests =
    testList "generateRandomSequenceTests" [
        testCase "length = 0" <| fun () ->
            let act = generateRandomSequence 0 |> List.ofSeq
            let exp = []

            Assert.Equal("", exp, act)

        testCase "length = 1" <| fun () ->
            let act = generateRandomSequence 1 |> List.ofSeq
            let exp = [0]

            Assert.Equal("", exp, act)

        testCase "length = 10" <| fun () ->
            let act = generateRandomSequence 10 |> List.ofSeq |> List.sort
            let exp = [0..9]

            Assert.Equal("", exp, act)
    ]
