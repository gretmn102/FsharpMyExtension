open Fuchu
open FsharpMyExtension
open FsharpMyExtension.FSharpExt

[<Tests>]
let simpleTest = 
    testCase "A simple test" <| 
        fun _ ->
            //Assert.Equal("2+3", 4, 2+3)
            (10, "20") |> function (a, b) as x -> Assert.Equal("comma a b = (a, b)", x, comma 10 b)
let ran = System.Random() |> fun x k n-> x.Next(k, n)

module ParserXpath =
    open Parser.Primitives
    open FsharpMyExtension.Either
    open Parser.XPathPar

    [<Tests>]
    let test3 =
        testList "xpath parse" [
            testCase "only name tag without attributes" <| fun () ->
                let k = run "a" Parser.XPathPar.Parser.name |> Either.isRight
                Assert.Equal("some", true, k)
            testCase "empty req" <| fun () ->
                let k = run "" Parser.XPathPar.Parser.name |> Either.isRight
                Assert.Equal("some", false, not k)
            testCase "*[@att='value'][@*='value'][@*]" <| fun () ->
                let xpath = "*[@att='value'][@*='value'][@*]"
                let exp = Right({ Name = None; Att = [(Some "att", Some "value"); (None, Some "value"); (None,None)];}, [])
                let act = run xpath Parser.XPathPar.Parser.res
                 
                Assert.Equal("some", exp, act)
            testCase "a[@href]" <| fun () ->
                let xpath = "a[@href]"
                let act = run xpath Parser.XPathPar.Parser.res
                Assert.Equal(sprintf "%A" act, true, Either.isRight act)
               ]


module ParserStringTest =
    open Parser
    open Parser.ParserString
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
    
    module SeqTests =
        [<Tests>]
        let seqTrav = travBaseTests "Seq.seqEitherBase" Seq.seqEither
        [<Tests>]
        let testLaziness = 
            testCase "lazyness test" <| fun () ->
                let counter = ref 0
                do seq{ yield Right 1; incr counter; yield Left -1; incr counter; yield Right 3; incr counter; } |> Seq.seqEither |> ignore
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

module T =
    open HtmlAgilityPack
    open Parser
    open Parser.Primitives
    open Parser.ParHtmlNode
    open FsharpMyExtension.HtmlNode
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
    
    ()

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
[<EntryPoint>]
let main arg =
    defaultMainThisAssembly arg
