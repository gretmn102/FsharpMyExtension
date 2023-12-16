open Fuchu

module ParserXpath =
    open FParsec

    open FsharpMyExtension
    open FsharpMyExtension.HtmlAgilityPackExt
    open FsharpMyExtension.Either
    open FsharpMyExtension.XPathLimited
    open FsharpMyExtension.FParsecExt

    [<Tests>]
    let ExecTest =
        testList "HtmlNode.isMatch" [
            testCase "base case" <| fun () ->
                let txt = "some text"
                let nod = HtmlAgilityPack.HtmlNode.CreateNode (sprintf "<a>%s</a>" txt)
                let p : HtmlAgilityPack.HtmlNode -> _ =
                    HtmlNode.isMatchRaw { Name = None; Att = []; Text = Some txt }
                Assert.Equal("'*[text()='%s']' = '<a>%s</a>'", true, p nod)
            testCase "2 case" <| fun () ->
                let htmlContent =
                    [
                        "<root>"
                        "  <a atr=\"attrVal\">a1</a>"
                        "  <a atr=\"attrVal\">a2</a>"
                        "</root>"
                    ] |> String.concat "\n"
                let act =
                    htmlContent
                    |> HtmlDocument.loadHtml
                    |> fun x ->
                        x.DocumentNode.SelectSingleNode "//*[text()='a2']"
                        |> HtmlNode.isMatch "a[@atr='attrVal'][text()='a2']"
                Assert.Equal("", true, act)
       ]

    [<Tests>]
    let parserTest =
        testList "xpath parse" [
            testCase "empty req" <| fun () ->
                let k = runEither Parser.pname "" |> Either.isRight
                Assert.Equal("some", false, not k)
            testCase "only name tag without attributes" <| fun () ->
                let k = runEither Parser.pname "a" |> Either.isRight
                Assert.Equal("some", true, k)
            testCase "a[@href]" <| fun () ->
                let xpath = "a[@href]"
                let act = runEither Parser.res xpath
                Assert.Equal(sprintf "%A" act, true, Either.isRight act)
            testCase "*[@att='value'][@*='value'][@*]" <| fun () ->
                let xpath = "*[@att='value'][@*='value'][@*]"
                let exp = Right { Name = None; Att = [(Some "att", Some "value"); (None, Some "value"); (None,None)]; Text = None}
                let act = runEither Parser.res xpath
                Assert.Equal("some", exp, act)

            testCase "any tag with some text: *[text()='some text in node']" <| fun () ->
                let xpath = "*[text()='some text in node']"
                let exp = Right { Name = None; Att = []; Text = Some "some text in node"}
                let act = runEither Parser.res xpath
                Assert.Equal("", exp, act)
            testCase "*[@att='value'][text()='some text in node']" <| fun () ->
                let xpath = "*[@att='value'][text()='some text in node']"
                let exp = Right { Name = None; Att = [(Some "att", Some "value");]; Text = Some "some text in node"}
                let act = runEither Parser.res xpath
                Assert.Equal("", exp, act)
            testCase "text between attributes: *[@att='value'][text()='some text in node'][@att2='val']" <| fun () ->
                let xpath = "*[@att='value'][text()='some text in node'][@att2='val']"
                let exp = Right { Name = None; Att = [(Some "att", Some "value"); Some "att2", Some "val" ]; Text = Some "some text in node"}
                let act = runEither Parser.res xpath
                Assert.Equal("", exp, act)
            testCase "two text: *[text()='some text in node'][text()='txt2']" <| fun () ->
                let xpath = "*[text()='some text in node'][text()='txt2']"
                //let exp = Right({ Name = None; Att = [(Some "att", Some "value"); Some "att2", Some "val" ]; Text = Some "some text in node"}, [])
                let act = runEither Parser.res xpath
                Assert.Equal("", true, Either.isLeft act)
        ]

    [<Tests>]
    let showsTest =
        testList "xpath parse" [
            testCase "first" (fun () ->
                let exp = "self::node()[name()='a'][@atr][text()='a1']"
                let act =
                    Parser.run "a[@atr][text()='a1']"
                    |> Either.getOrDef' (failwithf "%A")
                    |> ShowReq.showSelf
                Assert.Equal("", exp, act)
            )
            testCase "second" (fun () ->
                let exp = "self::node()[name()='tag'][@attrname1][@attrname2='attrVal2'][@*='attrVal3'][text()='text in node']"
                let act =
                    Parser.run "tag[@attrname1][@attrname2='attrVal2'][@*='attrVal3'][text()='text in node']"
                    |> Either.getOrDef' (failwithf "%A")
                    |> ShowReq.showSelf
                Assert.Equal("", exp, act)
            )
            testCase "second" <| fun () ->
                let input =
                    { Name = Some "tbody"
                      Att = [(Some "id", Some "post_20422607"); (Some "class", Some "row2")]
                      Text = None }
                let exp = "tbody[@id='post_20422607'][@class='row2']"
                let act = ShowReq.show input
                Assert.Equal("", exp, act)
        ]

module ListTests =
    open FsharpMyExtension

    [<Tests>]
    let groupBySeqTest =
        testList "groupBySeqTest" [
            testCase "base" <| fun () ->
                Assert.Equal("", [], List.groupBySeq (=) [])
            testCase "one" <| fun () ->
                let xs = [1]
                Assert.Equal("", [xs], List.groupBySeq (=) xs)
            testCase "many" <| fun () ->
                let xs = [[1;1]; [2]; [1; 1]; [3; 3;3]; [4; 4]; [5;5;5]]
                Assert.Equal("", xs, List.groupBySeq (=) (List.concat xs))
            testCase "many with function" <| fun () ->
                let xs = [[1;1]; [2]; [1; 1]; [3; 3; 3]; [4; 4]; [5;5;5]]
                xs |> List.mapFold (fun st -> List.mapFold (fun st x -> (st, x), st + 1) st) 0
                |> fst |> fun xs ->
                Assert.Equal("", xs, List.groupBySeq (fun x y -> snd x = snd y) (List.concat xs))
            testCase "many2 with function" <| fun () ->
                let xs = [ [1,1; 2,1;]; [3,2; 4,2] ]
                Assert.Equal("", xs, List.groupBySeq (fun x y -> snd x = snd y) (List.concat xs))
       ]
    [<Tests>]
    let circleTest =
        let circle count =
            List.circle >> Seq.take count >> List.ofSeq
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
                let xs = List.numerate None xs |> List.ofSeq
                let expected, act = List.sortBy fst xs, List.sortBy snd xs
                Assert.Equal(sprintf "%s error" lab, expected, act)
        testList "numerateTest" [
            testCase "base case" <| fun () ->
                Assert.Equal("", [], List.numerate None [])
            test "digits" [0..9]
            test "tens" [0..99]
            test "hundreds" [0..999]
            testCase "random case" <| fun () ->
                let xs = [1;3;5;1;3;30;20;10;30;40;50;10;323]
                let x =
                    xs |> List.numerate None |> List.sortBy snd
                    |> List.zip xs
                    |> List.tryFind (fun (x, y) -> x <> fst y)
                Assert.Equal(sprintf "%A" xs, None, x)
            testCase "print function (_ -> \".\")" <| fun () ->
                let xs = [0..30]
                let x =
                    List.numerate (Some (fun _ -> ".")) xs
                    |> List.tryFind (snd >> fun x -> x.EndsWith "." |> not)
                Assert.Equal(sprintf "arg [0..30], must be [0,\"00.\"; 0,\"01.\"...]", None, x)
       ]

    [<Tests>]
    let concatSepTest =
        let inline gen (sep: char) xs =
            let xs = List.ofSeq xs
            let exp = String.concat (sep.ToString()) (List.map string xs)
            let act = List.concatSep sep xs |> System.String.Concat
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
    [<Tests>]
    let chooseFoldTest =
        let xs = [1..10]
        let stExp = List.sum xs
        let xsExp = List.filter isEven xs
        let act =
            xs
            |> List.chooseFold (fun st x ->
                let x' = if isEven x then Some x else None
                x', st + x
                ) 0
        testCase "chooseFoldTest base" <| fun _ ->
            Assert.Equal("", (xsExp, stExp), act)

    [<Tests>]
    let sepByTests =
        testList "sepByTests" [
            testCase "empty" <| fun _ ->
                Assert.Equal("", [], List.sepBy "+" [])
            testCase "1" <| fun _ ->
                Assert.Equal("", ["1"], List.sepBy "+" ["1"])
            testCase "2" <| fun _ ->
                Assert.Equal("", ["1"; "+"; "2"], List.sepBy "+" ["1"; "2"])
            testCase "many" <| fun _ ->
                let values =
                    List.init 5 (id >> string)
                let exp =
                    ["0"; "+"; "1"; "+"; "2"; "+"; "3"; "+"; "4"]
                Assert.Equal("", exp, List.sepBy "+" values)
        ]

module EitherTests =
    open FsharpMyExtension.Either
    [<Tests>]
    let seqEitherPseudoTest =
        let seqEitherPseudo = Either.seqEitherPseudo
        testList "seqEitherPseudoTest" [
            testCase "BaseCase" <| fun () ->
                let act =
                    Right Seq.empty |> seqEitherPseudo |> List.ofSeq
                Assert.Equal("", [], act)
            testCase "Case2" <| fun () ->
                let act =
                    Left "0" |> seqEitherPseudo |> List.ofSeq
                Assert.Equal("", [Left "0"], act)
            testCase "Case3" <| fun () ->
                let act =
                    Right (seq[ Left "0"; Right 1 ]) |> seqEitherPseudo |> List.ofSeq
                Assert.Equal("", [Left "0"; Right 1], act)
        ]

    [<Tests>]
    let collectTest =
        let collect = Either.collect
        testList "collectTest" [
            testCase "BaseCase" <| fun () ->
                let act =
                    Right 0 |> collect (Seq.singleton << Right) |> List.ofSeq
                Assert.Equal("", [Right 0], act)
            testCase "Case2" <| fun () ->
                let act =
                    Left "error" |> collect (Seq.singleton << Right) |> List.ofSeq
                Assert.Equal("", [Left "error"], act)
            testCase "Case3" <| fun () ->
                let act =
                    Right ()
                    |> collect (fun _ -> seq [ Right 0; Left "left"; Right 1 ])
                    |> List.ofSeq
                Assert.Equal("", [Right 0; Left "left"; Right 1], act)
        ]
    [<Tests>]
    let seqOptTest =
        let seqOpt = Either.seqOpt
        testList "seqOptTest" [
            testCase "BaseCase1" <| fun () ->
                let act =
                    seqOpt (Left "")
                Assert.Equal("", Some(Left ""), act)
            testCase "BaseCase2" <| fun () ->
                let act =
                    seqOpt (Right (Some ""))
                Assert.Equal("", Some(Right ""), act)
            testCase "BaseCase3" <| fun () ->
                let act =
                    seqOpt (Right None)
                Assert.Equal("", None, act)
        ]

    let travBaseTests testListName fn =
        testList testListName [
            testCase "single Left" <| fun () ->
                let x = "some"
                Assert.Equal("", Left x, fn [Left x])
            testCase "single Right" <| fun () ->
                let x = 0
                Assert.Equal("", Right [x], fn [Right x])
            testCase "seq with Left" <| fun () ->
                let x = "error"
                Assert.Equal("", Left x, fn [Right 0; Left x; Right 1])
            testCase "seq only with Right" <| fun () ->
                Assert.Equal("", Right [0..9], List.init 10 Right |> fn)
                ]
    module ListTests =
        [<Tests>]
        let listTrav = travBaseTests "List.seqEitherBase" List.seqEither
        [<Tests>]
        let partitionEither =
            let part = FsharpMyExtension.Either.List.partitionEithers
            testList "partitionEither" [
                testCase "empty" <| fun () ->
                    Assert.Equal("", ([],[]), part [])
                testCase "mix" <| fun () ->
                    let xs = [Left "1"; Right 2; Left "3"; Right 4]
                    let ys = part xs
                    Assert.Equal("", (["1"; "3";], [2; 4]), part xs)
                testCase "mix2" <| fun () ->
                    let xs = [Right "1"; Left 2; Right "3"; Left 4]
                    Assert.Equal("", ([2; 4], ["1"; "3";]), part xs)
                // testCase "seq with Left" <| fun () ->
                //     let x = "error"
                //     Assert.Equal("", Left x, fn [Right 0; Left x; Right 1])
                // testCase "seq only with Right" <| fun () ->
                //     Assert.Equal("", Right [0..9], List.init 10 Right |> fn)
                    ]


    module SeqTests =
        [<Tests>]
        let seqTrav = travBaseTests "Seq.seqEitherBase" Seq.seqEither
        [<Tests>]
        let testLaziness =
            testCase "lazyness test" <| fun () ->
                let counter = ref 0
                seq{ yield Right 1; incr counter;
                     yield Left -1; incr counter;
                     yield Right 3; incr counter; } |> Seq.seqEither |> ignore
                Assert.Equal("", 1, !counter)

    module OptionTests =
        [<Tests>]
        let isAnyOfTest =
            testList "Option.ofEither" [
                testCase "left" <| fun () ->
                    Assert.Equal("Left _ = None", None, Option.ofEither (Left ()))
                testCase "right" <| fun () ->
                    let x = 1
                    Assert.Equal("Right x = Some x", Some x, Option.ofEither (Right x))
                   ]
module OptionTests =
    // module Option =
    //     [<Tests>]
    //     let ofNullTest =
    //         let ofNull = FsharpMyExtension.Option.Option.ofNull
    //         testList "ofNullTest" [
    //             testCase "null case" <| fun () ->
    //                 Assert.Equal("", None, ofNull null)
    //             testCase "some case" <| fun () ->
    //                 let x = 1
    //                 Assert.Equal("", Some x, ofNull( box x) |> Option.map unbox)
    //        ]
    module Seq =
        let seqOpt = FsharpMyExtension.Seq.seqOpt
        [<Tests>]
        let seqOptTest =
            testList "seqOptTest" [
                testCase "base case" <| fun () ->
                    Assert.Equal("", Some [], seqOpt Seq.empty)
                testCase "many Some case" <| fun () ->
                    let xs = [0..9]
                    Assert.Equal("", Some xs, List.map Some xs |> seqOpt)
                testCase "has None case" <| fun () ->
                    let xs = seq{ yield Some 1; yield None; yield Some 3 }
                    Assert.Equal("", None, seqOpt xs)
                testCase "lazyness test" <| fun () ->
                    let counter = ref 0
                    let xs = seq{
                        yield Some 1; incr counter;
                        yield None; incr counter;
                        yield Some 3; incr counter; }
                    seqOpt xs |> ignore
                    Assert.Equal("", 1, !counter)
           ]

module TreeTest =
    open FsharpMyExtension
    [<Tests>]
    let VisualizeTest =
        testList "VisualizeTest" [
            testCase "base case" <| fun () ->
                let dummy =
                  LT
                    ("1",
                     seq
                       {yield LT ("2",seq { yield LT ("3",Seq.empty); yield LT ("4",Seq.empty)});
                        yield LT ("3",seq { yield LT ("4",Seq.empty)})})
                let exp =
                    [
                        "1"
                        "├─2"
                        "│ ├─3"
                        "│ └─4"
                        "└─3"
                        "  └─4"
                    ] |> String.concat "\n"
                let act = LazyTree.visualize (sprintf "%s") dummy
                Assert.Equal("", exp, act)
       ]

module LZTests =
    open FsharpMyExtension.ListZipper

    [<Tests>]
    let RemoveRTest =
        let next = ListZ.next >> Option.get
        let rem = ListZ.removeR >> Option.get
        testList "RemoveRTest" [
            testCase "base case" <| fun () ->
                Assert.Equal("", None, ListZ.ofList [1] |> ListZ.removeR)
            testCase "remove first elem of two" <| fun () ->
                let x, y = 1, 2
                let act = ListZ.ofList [x;y] |> rem
                Assert.Equal("", ListZ.ofList [y], act)
            testCase "remove second elem of two" <| fun () ->
                let x, y = 1, 2
                let act = ListZ.ofList [x;y] |> next |> rem
                Assert.Equal("", ListZ.ofList [x], act)
            testCase "remove 2/3" <| fun () ->
                let x, y, z = 1, 2, 3
                let act = ListZ.ofList [x;y;z] |> next |> rem
                let e = ListZ.ofList [x;z] |> next
                Assert.Equal("", e, act)
            testCase "remove 3/3" <| fun () ->
                let x, y, z = 1, 2, 3
                let act =
                    ListZ.ofList [x;y;z] |> next |> next |> rem
                let e = ListZ.ofList [x;y] |> next
                Assert.Equal("", e, act)
       ]

    [<Tests>]
    let mapiTest =
        testList "mapiTest" [
            testCase "base" <| fun () ->
                let input = [0..9]
                let lz = ListZ.ofList input
                let act =
                    lz
                    |> ListZ.next |> Option.get
                    |> ListZ.mapi (fun i x -> i)
                    |> ListZ.toList
                Assert.Equal("", input, act)
        ]

    [<Tests>]
    let mapStartMidEndTest =
        testList "mapStartMidEndTest" [
            testCase "base" <| fun () ->
                let lz = ListZ.ofList [1..4]
                let act =
                    lz
                    |> ListZ.next |> Option.get
                    |> ListZ.mapStartMidEnd
                        (fun isCurrent x -> -1)
                        (fun isCurrent x  -> 0)
                        (fun isCurrent x -> 1)
                    |> ListZ.toList
                let exp = [-1; 0; 0; 1]
                Assert.Equal("", exp, act)
        ]

module LZCTests =
    open FsharpMyExtension.ListZipperCircle2
    [<Tests>]
    let RemoveRTest =
        let rem = LZC.removeR >> Option.get
        testList "LZC.RemoveR" [
            testCase "base case" <| fun () ->
                Assert.Equal("", None, LZC.ofList [1] |> LZC.removeR)
            testCase "remove first elem of two" <| fun () ->
                let x, y = 1, 2
                let act = LZC.ofList [x;y] |> rem
                Assert.Equal("", LZC.ofList [y] |> LZC.next, act)
            testCase "remove 2/2" <| fun () ->
                let x, y = 1, 2
                let act = LZC.ofList [x;y] |> LZC.next |> rem
                let e = LZC.ofList [x] |> LZC.next
                Assert.Equal("", e, act)
            testCase "remove 2/3" <| fun () ->
                let x, y, z = 1, 2, 3
                let act = LZC.ofList [x;y;z] |> LZC.next |> rem
                let e = LZC.ofList [x;z] |> LZC.next
                Assert.Equal("", e, act)
            testCase "remove 3/3" <| fun () ->
                let x, y, z = 1, 2, 3
                let act = LZC.ofList [x;y;z] |> LZC.next |> LZC.next |> rem
                let e = LZC.ofList [x;y] |> LZC.next |> LZC.next
                Assert.Equal("", e, act)
       ]
    let removeLTest =
        let rem = LZC.removeL >> Option.get
        testList "LZC.RemoveL" [
            testCase "base case" <| fun () ->
                Assert.Equal("", None, LZC.ofList [1] |> LZC.removeL)
            testCase "remove first elem of two" <| fun () ->
                let x, y = 1, 2
                let act = LZC.ofList [x;y] |> rem
                Assert.Equal("", LZC.ofList [y] |> LZC.prev, act)
            testCase "remove 2/2" <| fun () ->
                let x, y = 1, 2
                let act = LZC.ofList [x;y] |> LZC.prev |> rem
                let e = LZC.ofList [x] |> LZC.prev
                Assert.Equal("", e, act)
            testCase "remove 2/3" <| fun () ->
                let x, y, z = 1, 2, 3
                let act = LZC.ofList [x;y;z] |> LZC.prev |> rem
                let e = LZC.ofList [x;z] |> LZC.prev
                Assert.Equal("", e, act)
            testCase "remove 3/3" <| fun () ->
                let x, y, z = 1, 2, 3
                let act = LZC.ofList [x;y;z] |> LZC.prev |> LZC.prev |> rem
                let e = LZC.ofList [x;y] |> LZC.prev |> LZC.prev
                Assert.Equal("", e, act)
        ]

module Path =
    open FsharpMyExtension.Path
    [<Tests>]
    let ChangeFileNameWithoutExtTest =
        testList "ChangeFileNameWithoutExtTest" [
            testCase "base case" <| fun () ->
                Assert.Equal("", "file1",
                    changeFileNameWithoutExt (sprintf "%s1") "file")
            testCase "" <| fun () ->
                Assert.Equal("", "dir\\file1.fs",
                    changeFileNameWithoutExt (sprintf "%s1") "dir\\file.fs")
            testCase "" <| fun () ->
                Assert.Equal("", "e:\\dir\\file1.fs",
                    changeFileNameWithoutExt (sprintf "%s1") "e:\\dir\\file.fs")
       ]
    [<Tests>]
    let relativeTest =
        testList "relativeTest" [
            testCase "test 1" <| fun _ ->
                let path = @"E:\Project\SteamFeedsToDiscord\SteamFeedsToDiscord\SteamFeedsToDiscord\SteamFeedsToDiscord.fsproj"
                let currentDir = @"E:\Project\SteamFeedsToDiscord\Test\Test"

                let exp = @"..\..\SteamFeedsToDiscord\SteamFeedsToDiscord\SteamFeedsToDiscord.fsproj"
                let act = relative path currentDir
                Assert.Equal("", exp, act)
            testCase "test 2" <| fun _ ->
                let path = @"E:\Project\SteamFeedsToDiscord\Test\Test\bin\Debug\net461\FParsec.dll"
                let dir = @"E:\Project\SteamFeedsToDiscord\Test\Test\bin\Debug\net461"
                let exp = "FParsec.dll"
                let act = relative path dir
                Assert.Equal("", exp, act)
            testCase "test 3" <| fun _ ->
                let path = @"e:\Project\Parsers\Expr evaluation\Expr evaluation.suo"
                let dir = @"e:\Project\RenpyPseudo\Test\"
                let exp = @"..\..\Parsers\Expr evaluation\Expr evaluation.suo"
                let act = relative path dir
                Assert.Equal("", exp, act)
            testCase "test 4" <| fun _ ->
                let path = @"e:\Project\Parsers\Expr evaluation"
                let dir = @"e:\Project\RenpyPseudo\Test\"
                let exp = @"..\..\Parsers\Expr evaluation"
                let act = relative path dir
                Assert.Equal("", exp, act)
        ]

    [<Tests>]
    let getExtensionTests =
        testList "getExtensionTests" [
            testCase "empty" <| fun _ ->
                Assert.Equal("", "", getExtension "")
            testCase "http" <| fun _ ->
                let act = getExtension "http://localhost:99/https://img0.joyreactor.cc/pics/post/%D0%B6%D1%83%D1%80%D0%BD%D0%B0%D0%BB-%22%D0%9A%D1%80%D0%BE%D0%BA%D0%BE%D0%B4%D0%B8%D0%BB%22-%D1%81%D1%82%D0%B0%D1%80%D1%8B%D0%B5-%D0%B8%D0%B7%D0%B4%D0%B0%D0%BD%D0%B8%D1%8F-6285522.jpeg"
                let exp = ".jpeg"
                Assert.Equal("", exp, act)
            testCase "without dot" <| fun _ ->
                let act = getExtension "abcd efg"
                Assert.Equal("", "", act)
            testCase "first dot" <| fun _ ->
                let input = ".abcd asf efg"
                let act = getExtension input
                Assert.Equal("", input, act)
            testCase "null" <| fun _ ->
                Assert.Raise("", typeof<System.NullReferenceException>, (fun () -> getExtension null |> ignore))
        ]

    [<Tests>]
    let changeExtTests =
        testList "changeExtTests" [
            testCase "base" <| fun () ->
                let input = @"E:\Project\SmallTask\sandbox3.fsx"
                let exp = @"E:\Project\SmallTask\sandbox3.txt"
                let act = changeExt ".txt" input

                Assert.Equal("", exp, act)
        ]

module Comb =
    open FsharpMyExtension
    open FsharpMyExtension.Combinatorics
    [<Tests>]
    let packTest =
        testList "paTest" [
            testCase "base case" <| fun () ->
                // let exp = Comb.comb 3 [1..6] |> LazyTree.unpack |> List.ofSeq
                let exp =
                    [[1; 2; 3]; [1; 2; 4]; [1; 2; 5]; [1; 2; 6]; [1; 3; 4]; [1; 3; 5]; [1; 3; 6];
                     [1; 4; 5]; [1; 4; 6]; [1; 5; 6]; [2; 3; 4]; [2; 3; 5]; [2; 3; 6]; [2; 4; 5];
                     [2; 4; 6]; [2; 5; 6]; [3; 4; 5]; [3; 4; 6]; [3; 5; 6]; [4; 5; 6]]
                let act = LazyTree.pack exp |> LazyTree.unpack |> List.ofSeq
                Assert.Equal("", exp, act)
       ]
module SeqTests =
    open FsharpMyExtension

    [<Tests>]
    let concatSepTest =
        let inline gen (sep: char) xs =
            let xs = List.ofSeq xs
            let exp = String.concat (sep.ToString()) (List.map string xs)
            let act = Seq.concatSep sep xs |> System.String.Concat
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
                let act = Array.ofSeq (Seq.concatSep '+' "a")
                Assert.Equal("", exp, act)
            )
            testCase "testCase2" (fun _ ->
                let exp = Array.ofSeq "a+b"
                let act = Array.ofSeq (Seq.concatSep '+' "ab")
                Assert.Equal("", exp, act)
            )
            testCase "testCase3" (fun _ ->
                let exp = Array.ofSeq "a+b+c"
                let act = Array.ofSeq (Seq.concatSep '+' "abc")
                Assert.Equal("", exp, act)
            )
            testCase "test consistency" (fun _ ->
                let xs = Seq.concatSep '+' "abcdef"
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
                let act = Seq.generateRandomSequence 0 |> List.ofSeq
                let exp = []

                Assert.Equal("", exp, act)

            testCase "length = 1" <| fun () ->
                let act = Seq.generateRandomSequence 1 |> List.ofSeq
                let exp = [0]

                Assert.Equal("", exp, act)

            testCase "length = 10" <| fun () ->
                let act = Seq.generateRandomSequence 10 |> List.ofSeq |> List.sort
                let exp = [0..9]

                Assert.Equal("", exp, act)
        ]

[<EntryPoint>]
let main arg =
    defaultMainThisAssembly arg
