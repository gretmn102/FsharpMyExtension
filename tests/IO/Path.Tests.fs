module FsharpMyExtension.IO.Path.Tests
open Fuchu

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
