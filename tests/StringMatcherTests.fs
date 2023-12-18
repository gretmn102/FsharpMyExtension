module StringsMatcherTest
open Fuchu

open FsharpMyExtension.StringsMatcher
open FsharpMyExtension.Containers

[<Tests>]
let toDicTest =
    testList "toDicTest" [
        testCase "first" <| fun () ->
            let input =
                [
                    "a", "first"
                    "ab", "second"
                    "ac", "third"
                ]

            let exp =
                Map
                  [('a',
                    Dic
                      (Some "first",
                       Map
                         [('b', Dic (Some "second", Map []));
                          ('c', Dic (Some "third", Map []))]))]
            let input2 =
                [
                    "second", "second"
                    "first", "first"
                    "firmest", "firmest"
                    "sec", "sec"
                ]
            Assert.Equal("", exp, toDic input)
    ]

[<Tests>]
let lookupTests =
    testList "lookupTests" [
        testCase "base" <| fun () ->
            let input =
                [|"a"; "a"; "b"; "b"; "c"|]
                |> Array.mapi (fun i s -> s, i)

            let len = input.Length

            Assert.Equal("", Some('a', 2), lookup(0, len, 0, input))
            Assert.Equal("", Some('a', 1), lookup(0, 1, 0, input))
            Assert.Equal("", Some('b', 4), lookup(2, len, 0, input))
            Assert.Equal("", Some('c', len), lookup(4, len, 0, input))
    ]

let toDicStringsSlow xs =
    let dicEmpty = Dic(None, Map.empty)
    let rec slow i (xs:list<string * _ list>) =
        if Seq.isEmpty xs then dicEmpty
        else
            let emptys, others =
                xs
                |> List.partition (
                    fst >> (fun (x:string) -> not (i < x.Length) ) )
            let emptys =
                List.collect snd emptys
                |> function
                    | [x] -> Some x
                    | [] -> None
                    | xs -> failwithf "в списке есть одинаковые ключи:\n%A" xs
            others
            |> List.groupBy (fst >> (fun x -> x.[i]))
            |> List.fold (fun m (k, v) ->
                Map.add k (slow (i + 1) v) m) Map.empty
            |> fun m -> Dic(emptys, m)
    xs
    |> List.map (fun (x, y) -> x, [y])
    |> slow 0
    |> fun (Dic(_, m)) -> m

[<Tests>]
let toDicStringsTests =
    testList "toDicStringsTests" [
        testCase "base" <| fun () ->
            let sample =
                [
                    "abc"
                    "abcd"
                    "abd"
                    "acd"
                    "acdef"
                    "acde"
                    "bcd"
                ]
                |> List.sort
                |> List.mapi (fun i x -> x, i)

            let exp =
                sample
                |> toDicStringsSlow

            Assert.Equal("", exp, toDicStrings (Array.ofList sample))
        testCase "same words" <| fun () ->
            let sample =
                [
                    "ab"
                    "abc"
                    "abc"
                ]
                |> List.sort
                |> List.mapi (fun i x -> x, i)

            try
                toDicStrings (Array.ofList sample) |> ignore
                failtest "the list contains the same words must fail"
            with e ->
                Assert.Equal("fail", "the list contains the same words: (\"abc\", 2)", e.Message)
    ]

let timeDiagnostic fn =
    let d1 = System.DateTime.Now
    let x = fn()
    let d2 = System.DateTime.Now
    x, d2 - d1

let toDicStringsTestsSlow =
    testList "toDicStringsTestsSlow" [
        testCase "base" <| fun () ->
            let getWordsLocal () =
                let russianDictionaryPath = @"e:\Project\YetAnotherSpellCheckerServer\output\Russian-English Bilingual.txt"

                if System.IO.File.Exists russianDictionaryPath then
                    let words =
                        System.IO.File.ReadLines russianDictionaryPath
                        |> Seq.sort
                        |> Seq.mapi (fun i x -> x, i)

                    Some words
                else
                    None

            let getWordsNet () =
                use c = new System.Net.WebClient()

                printfn "Downloading dictionary from internet..."
                let words = c.DownloadString("https://raw.githubusercontent.com/danakt/russian-words/master/russian.txt")

                printfn "Done!"

                words.Split [|'\n'|]
                |> Array.sort
                |> Array.mapi (fun i x -> x, i)

            let words =
                getWordsLocal ()
                |> Option.defaultWith (fun () ->
                    getWordsNet ()
                    |> Seq.tail // remove ""
                )

            let exp, time =
                words
                |> List.ofSeq
                |> fun x ->
                    timeDiagnostic (fun () -> toDicStringsSlow x)

            printfn "toDicStrings: %A" time

            let act, time =
                words
                |> Array.ofSeq
                |> fun x ->
                    timeDiagnostic (fun () -> toDicStrings x)

            printfn "toDicStrings optimize: %A" time

            Assert.Equal("", exp, act)
    ]

// run toDicStringsTestsSlow |> ignore

[<Tests>]
let runOnListNotGreedyTest =
    let input =
        [
            "abc", "third"
            "a", "first"
            "ab", "second"
            "c", "fourth"
        ]

    let dic = toDic input

    [
        Some "first", runOnListNotGreedy dic (List.ofSeq "as")
        Some "first", runOnListNotGreedy dic (List.ofSeq "ab")
        Some "first", runOnListNotGreedy dic (List.ofSeq "ac")
        None, runOnListNotGreedy dic (List.ofSeq "b")
        Some "fourth", runOnListNotGreedy dic (List.ofSeq "c")
    ]
    |> List.mapi (fun i (exp, act) ->
        testCase (sprintf "%d" i) <| fun () ->
            Assert.Equal("", exp, act) )
    |> testList "runOnListNotGreedyTest"

[<Tests>]
let runOnListGreedyTest =
    let input =
        [
            "abc", "third"
            "a", "first"
            "ab", "second"
            "c", "fourth"
        ]

    let dic = toDic input

    [
        Some "third", runOnListGreedy dic (List.ofSeq "abc")
        Some "first", runOnListGreedy dic (List.ofSeq "a")
        Some "second", runOnListGreedy dic (List.ofSeq "ab")
        Some "fourth", runOnListGreedy dic (List.ofSeq "c")
        Some "first", runOnListGreedy dic (List.ofSeq "ac")
        None, runOnListGreedy dic (List.ofSeq "b")
    ]
    |> List.mapi (fun i (exp, act) ->
        testCase (sprintf "%d" i) <| fun () ->
            Assert.Equal("", exp, act) )
    |> testList "runOnListGreedyTest"

[<Tests>]
let runOnListGreedyTest2 =
    let dic =
        ["ab"; "abcd"]
        |> List.map (fun x -> x, x)
        |> toDic
    testCase "runOnListGreedyTest2" (fun () ->
        Assert.Equal("", Some "abcd", runOnListGreedy dic (List.ofSeq "abcd"))
    )

open FParsec

open FsharpMyExtension.FParsecExt
open FsharpMyExtension.StringsMatcher.FParsec

[<Tests>]
let keywordsLTest =
    let input =
        [
            "abc", "third"
            "a", "first"
            "ab", "second"
            "c", "fourth"
            "xyzSome", "xyz"
        ]
    let dic = toDic input

    let p = FParsec.keywordsL dic "keywords"

    let run' str =
        runEither p str

    [
        Right "third", run' "abc"
        Right "first", run' "a"
        Right "second", run' "ab"
        Right "fourth", run' "c"
        Right "first", runEither (p .>> pchar 'c') "ac"
        Right "xyz", run' "xyzSome"
        Right "xyzSom", runEither (attempt p <|> pstring "xyzSom") "xyzSom"
        Left "", (run' "b" |> Either.mapLeft (fun _ -> ""))
        Right "fourth", run' "c"
    ]
    |> List.mapi (fun i (exp, act) ->
        testCase (sprintf "%d" i) <| fun () ->
            Assert.Equal("", exp, act) )
    |> testList "keywordsLTest"

[<Tests>]
let keywordsLTest2 =
    let p dic =
        keywordsL dic "keyword" .>>. manySatisfy (fun _ -> true)
        <|> (many1Satisfy (fun _ -> true) |>> fun x -> "", x)

    testList "keywordsLTest2" [
        let dic =
            [ "bb" ]
            |> List.map (fun x -> x, x)
            |> toDic
        testCase "1" (fun () ->
            Assert.Equal("", Right ("", "b"), runEither (p dic) "b")
        )
        testCase "2" (fun () ->
            Assert.Equal("", Right ("bb", ""), runEither (p dic) "bb")
        )
        testCase "3" (fun () ->
            Assert.Equal("", Right ("bb", "b"), runEither (p dic) "bbb")
        )
        let dic =
            [
                "ab"
                "abc"
            ]
            |> List.map (fun x -> x, x)
            |> toDic
        testCase "test on greedy" (fun () ->
            Assert.Equal("", Right ("abc", ""), runEither (p dic) "abc")
        )
        testCase "test on greedy 2" (fun () ->
            Assert.Equal("", Right ("abc", "d"), runEither (p dic) "abcd")
        )
        let dic =
            [
                "ab"
                "abcd"
            ]
            |> List.map (fun x -> x, x)
            |> toDic
        testCase "test on greedy 3" (fun () ->
            Assert.Equal("", Right ("ab", ""), runEither (p dic) "ab")
        )
        testCase "test on greedy 4" (fun () ->
            Assert.Equal("", Right ("abcd", ""), runEither (p dic) "abcd")
        )
        testCase "test on greedy 5" (fun () ->
            Assert.Equal("", Right ("abcd", "e"), runEither (p dic) "abcde")
        )
        let dic =
            [
                "поз"
                "поза"
                "поздр"
            ] |> List.map (fun x -> x, x) |> toDic
        testCase "test on  5" (fun () ->
            Assert.Equal("", Right ("поз", "д"), runEither (p dic) "позд")
        )
    ]

[<Tests>]
let pcharFromDicTest =
    let m =
        [|
            "a"
            "ab"
            "abc"
            "b"
        |]
        |> Array.map (fun x -> x, x)
        |> toDicStrings

    testList "pcharFromDicTest" [
        testCase "`pcharFromDic id \"label\" m`" (fun () ->
            let act = runEither (pcharFromDic id "label" m) "a"
            let exp =
              ('a',
               Dic
                 (Some "a",Map [('b', Dic (Some "ab",Map [('c', Dic (Some "abc",Map []))]))]))
            Assert.Equal("", Right exp, act)
        )
        testCase "`pcharFromDic (fun x -> 'c') \"label\" m`" (fun () ->
            let act = runEither (pcharFromDic (fun x -> 'c') "label" m) "a"
            let exp =
                [
                    "Error in Ln: 1 Col: 1"
                    "a"
                    "^"
                    "Expecting: label"
                    ""
                ] |> String.concat "\r\n"
            Assert.Equal("", Left exp, act)
        )
    ]

module Serializator =
    open FsharpMyExtension.StringsMatcher
    open FsharpMyExtension.StringsMatcher.Serializator

    [<Tests>]
    let serializatorTest =
        testList "StringsMatcher.Serializator test" [
            testCase "serialize and deserialize" <| fun () ->
                let dic =
                    [|
                        "a"
                        "abc"
                        "b"
                        "c"
                        "cab"
                    |]
                    |> Array.map (fun x -> x, x)

                let exp = toDicStrings dic

                let data = exp |> serialize
                let act = deserialize data

                Assert.Equal("", exp, act)
        ]
