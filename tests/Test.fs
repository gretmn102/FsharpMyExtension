open Fuchu

module ParserXpath =
    open FParsec

    open FsharpMyExtension
    open FsharpMyExtension.HtmlAgilityPackExt
    open FsharpMyExtension.Containers.Either
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
        let seqOpt = FsharpMyExtension.Collections.Seq.seqOpt
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
    open FsharpMyExtension.Collections
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

[<EntryPoint>]
let main arg =
    defaultMainThisAssembly arg
