open Fuchu
open FsharpMyExtension.FSharpExt

let ran = System.Random() |> fun x k n -> x.Next(k, n)
module FsharpExt =
    [<Tests>]
    let For'Test =
        testList "For'Test" [
            testCase "base case" <| fun () ->
                let st = -1
                Assert.Equal("", st, for' 1 0 (fun _ _ -> 2) st)
            testCase "fact" <| fun () ->
                Assert.Equal("", List.reduce (*) [1..12], for' 1 12 (*) 1)
       ]
module ParserXpath =
    open FsharpMyExtension.HtmlAgilityPackExt
    open FsharpMyExtension.Either
    open FParsec
    open FsharpMyExtension.XPathLimited
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
    let run str p =
        match run p str with
        | Success(x, _, _) -> Right x
        | Failure(x, _, _) -> Left x
    [<Tests>]
    let parserTest =
        testList "xpath parse" [
            testCase "empty req" <| fun () ->
                let k = run "" Parser.pname |> Either.isRight
                Assert.Equal("some", false, not k)
            testCase "only name tag without attributes" <| fun () ->
                let k = run "a" Parser.pname |> Either.isRight
                Assert.Equal("some", true, k)
            testCase "a[@href]" <| fun () ->
                let xpath = "a[@href]"
                let act = run xpath Parser.res
                Assert.Equal(sprintf "%A" act, true, Either.isRight act)
            testCase "*[@att='value'][@*='value'][@*]" <| fun () ->
                let xpath = "*[@att='value'][@*='value'][@*]"
                let exp = Right { Name = None; Att = [(Some "att", Some "value"); (None, Some "value"); (None,None)]; Text = None}
                let act = run xpath Parser.res
                Assert.Equal("some", exp, act)

            testCase "any tag with some text: *[text()='some text in node']" <| fun () ->
                let xpath = "*[text()='some text in node']"
                let exp = Right { Name = None; Att = []; Text = Some "some text in node"}
                let act = run xpath Parser.res
                Assert.Equal("", exp, act)
            testCase "*[@att='value'][text()='some text in node']" <| fun () ->
                let xpath = "*[@att='value'][text()='some text in node']"
                let exp = Right { Name = None; Att = [(Some "att", Some "value");]; Text = Some "some text in node"}
                let act = run xpath Parser.res
                Assert.Equal("", exp, act)
            testCase "text between attributes: *[@att='value'][text()='some text in node'][@att2='val']" <| fun () ->
                let xpath = "*[@att='value'][text()='some text in node'][@att2='val']"
                let exp = Right { Name = None; Att = [(Some "att", Some "value"); Some "att2", Some "val" ]; Text = Some "some text in node"}
                let act = run xpath Parser.res
                Assert.Equal("", exp, act)
            testCase "two text: *[text()='some text in node'][text()='txt2']" <| fun () ->
                let xpath = "*[text()='some text in node'][text()='txt2']"
                //let exp = Right({ Name = None; Att = [(Some "att", Some "value"); Some "att2", Some "val" ]; Text = Some "some text in node"}, [])
                let act = run xpath Parser.res
                Assert.Equal("", true, Either.isLeft act)
        ]
    open FsharpMyExtension
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
        ]

module ParserHtmlNodeTest =
    open HtmlAgilityPack
    open FsharpMyExtension.HtmlAgilityPackExt
    open Parser.ParHtmlNode
    [<Tests>]
    let isEmptyInnerTextTest =
        testList "isEmptyInnerTextTest" [
            testCase "base case" <| fun () ->
                let emptyNode = HtmlNode.CreateNode " \n \r \n\r"
                let act = HtmlNode.IsNullOrWhiteSpace emptyNode
                Assert.Equal(@"expected ' \n \r \n\r' is empty", true, act)
            testCase "" <| fun () ->
                let emptyNode = HtmlNode.CreateNode " \n \r \n\rb"
                let act = HtmlNode.IsNullOrWhiteSpace emptyNode
                Assert.Equal(@"expected ' \n \r \n\rb' is not empty", false, act)
       ]
module ParserStringTest =
    open Parser
    open Parser.Primitives
    open Parser.ParserString
    open FsharpMyExtension.Either
    [<Tests>]
    let isAnyOfTest =
        testList "ParserString.isAnyOf" [
            testCase "empty" <| fun () ->
                Assert.Equal("must be False for any arg", false, (ParserString.isAnyOf "") '1')
            testCase "closure" <| fun () ->
                let i = ref 0
                let fn = isAnyOf (seq { incr i; yield 'a' })
                do List.init 10 (fun _ -> fn 'a') |> ignore
                Assert.Equal("some2", 1, !i)
               ]

    [<Tests>]
    let pint32Test =
        let test lab num s =
            testCase lab <| fun () ->
                let e = Right(num, List.ofSeq s)
                let act = run (sprintf "%d%s" num s) pint32
                Assert.Equal("", e, act)
        testList "pint32Test" [
            testCase "empty stream" <| fun () ->
                let act = run "" pint32
                Assert.Equal(sprintf "%A" act, true, act |> Either.isLeft)
            test "big number" 1234567899 "9123"
            test "" 123 "a1234"
            test "" -123 "a1234"
            test "" System.Int32.MinValue " some string"
            test "" System.Int32.MaxValue " some string"
            testCase "empty" <| fun () ->
                let s = "-abcd"
                let act = run s pint32
                Assert.Equal(sprintf "must be: %A, but %A" s act, true, act |> Either.isLeft)
            testCase "empty neg" <| fun () ->
                let s = "-abcd"
                let act = run s pint32
                Assert.Equal(sprintf "must be: %A, but %A" s act, true, act |> Either.isLeft)
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
module EitherTests =
    open FsharpMyExtension.Either

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
module T =
    open HtmlAgilityPack
    open Parser
    open Parser.Primitives
    open Parser.ParHtmlNode
    open FsharpMyExtension.HtmlAgilityPackExt
    open FsharpMyExtension.Either
    let pUrlThumb =
        ParserString.pstringCI "background-image: url(" >>. ParserString.manyChars ((ParserString.pstringCI "&amp;" >>% '&') <|> (satisfy ((<>) ')') (k null) null)) .>> ParserString.pchar ')' .>> pend (fun _ -> "error then thumb def")
        |>> sprintf "https:%s"
    assert
        "background-image: url(//thumbs.hentai-foundry.com/thumb.php?pid=516456&amp;size=350)" |> List.ofSeq |> pUrlThumb = Right("https://thumbs.hentai-foundry.com/thumb.php?pid=516456&size=350", [])

    /// из "/pictures/user/Fredricton/510500/Bath-Mother-Pinup" извлекает ("Fredricton", 510500, "Bath-Mother-Pinup")
    let pUrlPicPage =
        let slash = ParserString.pchar '/'
        pipe3
            (ParserString.pstringCI "/pictures/user/" >>. ParserString.manySatisfy ((<>) '/') .>> slash)
            (ParserString.pint32 .>> slash)
            (ParserString.manySatisfy (fun _ -> true))
            (fun author id name -> author, id, name)
    type Pic =
        { UrlPicPage:string;
          UrlOrigin:string;
          Author:string;
          Tags:string list;
          UrlThumb:string;
          Name:string;
          Added:System.DateTime
          Request:string list
          Id:int
          }
    let pars =
        ws >>. takr "div[@class='thumb_square']" >>=
            sub (
                pipe4
                    (takr "div[@class='thumbTitle']" >>=
                        sub (takr "a[@href]" |>> fun node ->
                            HtmlNode.getAttVal "href" node |> fun href -> href |> List.ofSeq |> (pUrlPicPage |>> fun (_,id,_) -> id, href, HtmlNode.innerText node)) |> trav)
                    (takr "a[@class='thumbLink'][@href]" >>=
                        sub (takr "span[@title][@class][@style]" |>> (HtmlNode.getAttVal "style" >> List.ofSeq >> pUrlThumb)) |> trav)
                    (takr "div[@class='ratings_box']" >>=
                        sub (many (takr "span[@class][@title]") |>> List.map (HtmlNode.getAttVal "title")) )
                    (takr "a[@href]" |>> HtmlNode.innerText)
                    (fun (id, urlPicPage, title) thumbImg genres author ->
                            { UrlPicPage = urlPicPage;
                              UrlOrigin = null
                              Author = author
                              Tags = genres
                              UrlThumb = thumbImg
                              Name = title
                              Added = System.DateTime.MinValue
                              Request = []
                              Id = id }))

    [<Tests>]
    let isAnyOfTest =
        let node = HtmlNode.CreateNode "<div class='thumb_square'>\n        <div class='thumbTitle'>\n            <a href='/pictures/user/Calm/516456/Patreon-37'>Patreon #37</a>\n        </div>\n        <a class='thumbLink' href='/pictures/user/Calm/516456/Patreon-37'>\n            <span title='Patreon #37' class='thumb' style='background-image: url(//thumbs.hentai-foundry.com/thumb.php?pid=516456&amp;size=350)'></span>\n        </a>\n        <div class='ratings_box'>\n            <span class='rating lvl3' title='Nudity'>N</span>\n            <span class='rating lvl3' title='Sexual content'>Sx</span>\n            <span class='rating lvl2' title='Contains female nudity'>♀</span>\n        </div>\n        <a href='/user/Calm/profile'>Calm</a>\n    </div>"
        testList "extension of HtmlNode" [
            testCase "empty" <| fun () ->
                let res = pars [node]
                Assert.Equal("must be False for any arg", true, true)
            // testCase "satisfyError" <| fun () ->
            //     let f (x:Parser.Primitives.Pars<System.Collections.BitArray,System.Collections.BitArray>) = 1
            //     Assert.Equal("", "", sprintf "%A" r)
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

module ParserTests =
    module PrimitivesTests =
        open Parser.Primitives
        // Parser.Primitives.satisfy

        // [<Tests>]
        // let isAnyOfTest =
        //     testList "extension of HtmlNode" [
        //         testCase "empty" <| fun () ->
        //             let res = pars [node]
        //             Assert.Equal("must be False for any arg", true, true)
        //         // testCase "satisfyError" <| fun () ->
        //         //     let f (x:Parser.Primitives.Pars<System.Collections.BitArray,System.Collections.BitArray>) = 1
        //         //     Assert.Equal("", "", sprintf "%A" r)
        //            ]

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
module DateTimeTest =
    open FsharpMyExtension
    [<Tests>]
    let daysStepTest =
        testList "daysStepTest" [
            testCase "base case min" <| fun () ->
                let act = List.ofSeq (DateTime.daysStep -1 System.DateTime.MinValue)
                Assert.Equal("", [], act)
            testCase "base case max" <| fun () ->
                let act = List.ofSeq (DateTime.daysStep 1 System.DateTime.MaxValue)
                Assert.Equal("", [], act)
            testCase "one case min" <| fun () ->
                let d = System.DateTime.MinValue
                let exp = [d]
                let x = d.AddDays 1.
                let act = List.ofSeq (DateTime.daysStep -1 x)
                Assert.Equal("", exp, act)
            testCase "one case max" <| fun () ->
                let d = System.DateTime.MaxValue
                let exp = [d]
                let x = d.AddDays -1.
                let act = List.ofSeq (DateTime.daysStep 1 x)
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

module WebDownloader =
    open FsharpMyExtension
    open FsharpMyExtension.WebDownloader

module Show =
    open FsharpMyExtension
    open FsharpMyExtension.ShowList
    [<Tests>]
    let joinTest =
        testList "joinTest" [
            testCase "base case" <| fun () ->
                let exp = "Abram, Lyouis, Loid"
                let xs = [empty;empty;showString "Abram";showString "Lyouis";empty;empty;showString "Loid";empty;empty]
                let act =
                    joins (showString ", ") xs
                    |> show
                    |> System.String.Concat
                Assert.Equal("", exp, act)
       ]
    [<Tests>]
    let joinEmptyTest =
        testList "joinTest" [
            testCase "base case" <| fun () ->
                let exp = ", , Abram, Lyouis, , , Loid, , "
                let xs = [empty;empty;showString "Abram";showString "Lyouis";empty;empty;showString "Loid";empty;empty]
                let act =
                    joinEmpty ", " xs
                    |> show
                    |> System.String.Concat
                Assert.Equal("", exp, act)
       ]

module StringsMatcherTest =
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

    open FParsec
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
        let run p str =
            match FParsec.CharParsers.run p str with
            | Success(x, _, _) -> Right x
            | Failure(x, _, _) -> Left x
        let run' str =
            match FParsec.CharParsers.run p str with
            | Success(x, _, _) -> Right x
            | Failure(x, _, _) -> Left x


        [
            Right "third", run' "abc"
            Right "first", run' "a"
            Right "second", run' "ab"
            Right "fourth", run' "c"
            Right "first", run (p .>> pchar 'c') "ac"
            Right "xyz", run' "xyzSome"
            Right "xyzSom", run (attempt p <|> pstring "xyzSom") "xyzSom"
            Left "", (run' "b" |> Either.mapLeft (fun _ -> ""))
            Right "fourth", run' "c"
        ]
        |> List.mapi (fun i (exp, act) ->
            testCase (sprintf "%d" i) <| fun () ->
                Assert.Equal("", exp, act) )
        |> testList "keywordsLTest"

module ContentTypeTests =
    open FsharpMyExtension.Net.ContentType
    open FsharpMyExtension.Either
    // #load "ContentType.fs"
    // open Net.ContentType.Types
    // open Types

    [<Tests>]
    let contentTypeTest =
        testList "contentTypeTest" [
            testCase "1" (fun () ->
                let enc = System.Text.Encoding.UTF8
                let exp =
                  Right
                    { Typ = Application
                      Subtype = "sub"
                      Parameter = Some (Charset enc) }
                let act = Parser.start (sprintf "application/sub; charset=%s" enc.WebName)
                Assert.Equal("", exp, act) )
            testCase "2" (fun () ->
                let enc = System.Text.Encoding.UTF8
                let exp =
                  Right
                    { Typ = Text
                      Subtype = "css"
                      Parameter = Some (Charset enc) }
                let act = Parser.start (sprintf "text/css; charset=%s" enc.WebName)
                Assert.Equal("", exp, act) )
            testCase "3" (fun () ->
                let exp =
                  Right
                    { Typ = Text
                      Subtype = "css"
                      Parameter = None }
                let act = Parser.start "text/css"
                Assert.Equal("", exp, act) )
            testCase "3" (fun () ->
                let enc = System.Text.Encoding.UTF8
                let exp =
                  Right
                    { Typ = Text
                      Subtype = "html"
                      Parameter = Some (Charset enc) }
                let act = Parser.start (sprintf "text/html; charset=%s" enc.WebName)
                Assert.Equal("", exp, act) )
            testCase "3" (fun () ->
                let exp =
                  Right
                    { Typ = Image
                      Subtype = "svg+xml"
                      Parameter = None }
                let act = Parser.start "image/svg+xml"
                Assert.Equal("", exp, act) )
            testCase "unknown encoding" (fun () ->
                let act = Parser.start "text/html; charset=utf-82"
                Assert.Equal(".isLeft", true, Either.isLeft act) )
            testCase "custom parameter" (fun () ->
                let prmName, prmVal = "customParameter", "someValue"
                let exp =
                  Right
                    { Typ = Text
                      Subtype = "html"
                      Parameter = Some (CustomParameter (prmName, prmVal)) }
                let act = Parser.start (sprintf "text/html; %s=%s" prmName prmVal)
                Assert.Equal("", exp, act) )
        ]

    // run contentTypeTest

[<EntryPoint>]
let main arg =
    defaultMainThisAssembly arg
