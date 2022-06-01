module StringsMatcherTest
open Fuchu

open FsharpMyExtension.StringsMatcher
open FsharpMyExtension.Either

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
open FsharpMyExtension.StringsMatcher.FParsec

let runEither p str =
    match FParsec.CharParsers.run p str with
    | Success(x, _, _) -> Right x
    | Failure(x, _, _) -> Left x

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
        [
            "a"
            "ab"
            "abc"
            "b"
        ]
        |> List.map (fun x -> x, x)
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
                    [
                        "a"
                        "abc"
                        "b"
                        "c"
                        "cab"
                    ] |> List.map (fun x -> x, x)
                let exp = toDicStrings dic
                let data = exp |> serialize
                let act = deserialize data

                Assert.Equal("", exp, act)
        ]
