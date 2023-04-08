module ParserPrimitivesTest

open FsharpMyExtension
open FsharpMyExtension.Either
open Fuchu

// #if INTERACTIVE
// #load "ParserPrimitives2.fs"
// //# silentCd @"e:\Project\FsharpMyExtension\FsharpMyExtension\FsharpMyExtension\"
// //#load @"Tree.fs"
// //#load "Either.fs"
// #endif

open FsharpMyExtension.Tree
open Parser.Primitives

let change s = Left (true, Node(NotBack s, []))

let xs = [1..5]
[<Tests>]
let runTest =
    testCase "run; praw" <| fun () ->
        Assert.Equal("", ((List.tail xs, ()), List.head xs |> Right), run xs praw)

[<Tests>]
let preturnTest =
    testCase "preturn" <| fun () ->
        let x = 1
        Assert.Equal("", ((xs, ()), x |> Right), run xs (preturn x))
[<Tests>]
let satisfyTest =
    testList "satisfyTests" [
        testCase "error" <| fun () ->
            // satisfy
            Assert.Equal("", ((xs,()), notChange "some"), run xs (satisfy (k false) "some"))
        testCase "right" <| fun () ->
            Assert.Equal("", ((List.tail xs,()), List.head xs |> Right), run xs (satisfy (k true) null))
    ]
[<Tests>]
let pzeroTest =
    testCase "pzeroTest" <| fun () ->
        Assert.Equal("", ((xs,()), notChange ""), run xs pzero)

[<Tests>]
let bindTest =
    testList "(>>=) test; need: preturn, pzero" [
        testCase "error" <| fun () ->
            // satisfy
            Assert.Equal("", ((xs,()), notChange ""), run xs ( pzero >>= fun _ -> praw ))
        testCase "error2" <| fun () ->
            Assert.Equal("", ((List.tail xs,()), change ""), (run xs ( praw >>= fun _ -> pzero ) : Result<int,unit,_>))
        testCase "right" <| fun () ->
            let ys = xs |> List.skip 1
            Assert.Equal("", ((List.tail ys,()), (List.head xs, List.head ys) |> Right), run xs ( praw >>= fun x -> praw >>= fun y -> preturn (x, y) ))
    ]
[<Tests>]
let bindRightTest =
    testList "'(>>.)' test" [
        testCase "(>>.), praw, pzero" <| fun () ->
            Assert.Equal("", ((List.tail xs,()), change ""), (run xs (praw >>. pzero) : Result<int, int,_>))
        testCase "right" <| fun () ->
            let ys = xs |> List.skip 1
            Assert.Equal("",  ((List.tail ys,()), List.head ys |> Right), run xs (praw >>. praw))
    ]

[<Tests>]
let orTest =
    testList "'(<|>)' test; need: 'pzero', '(>>.)'" [
        testCase "(>>.), praw, pzero" <| fun () ->
            let expected = (xs,()), Left (false, Node(Or, [Tree.singleton <| NotBack ""; Tree.singleton <| NotBack ""]))
            Assert.Equal("", expected, (run xs (pzero <|> pzero) : Result<int,int,_>))
        testCase "sdf" <| fun () ->

            Assert.Equal("", (([],()), Left (true, Tree.singleton <| NotBack "1")), (run [1] (((praw .>> (pzero <?> "1")) <|> praw)) : Result<int, int,_>))
        testCase "right" <| fun () ->
            Assert.Equal("",  ((List.tail xs,()), List.head xs |> Right), run xs (pzero <|> praw))
    ]

[<Tests>]
let manyTest =
    let rec manyTrue p = (p >>= fun x -> manyTrue p |>> fun xs -> x::xs ) <|> preturn []
    testList "'many' test; need: 'satisfy', 'pzero'" [
        testCase "(>>.), praw, pzero" <| fun () ->
            let n = 3
            let ys = List.take n xs
            Assert.Equal("", ((List.skip n xs,()), Right ys), run xs (many (satisfy (Set.ofList ys |> flip Set.contains) null)))
        testCase "st, many pzero -> st, Right []" <| fun () ->
            Assert.Equal("", ((xs,()), Right []), run xs (many pzero))
        testCase "right" <| fun () ->
            let pdigit = (fun (n:int) -> satisfy ((=) n) (sprintf "%d" n))
            let p f = run [1;3] (f (pdigit 1 >>. pdigit 2 )) // ->  ([3], Left (true, [NotBack "2"]))
            Assert.Equal("",  p manyTrue, p many)
    ]

[<Tests>]
let attemptTest =
    testList "'attempt' test; need: 'praw', 'pzero', '(>>.)'" [
        //testCase "base" <| fun () ->
        testCase "base case `attempt (praw >>. pzero)` (state test)" <| fun () ->
            let actual = fst <| run xs (attempt (praw >>. pzero))
            Assert.Equal("", (xs,()), actual)
        testCase "attempt in attempt: `attempt (attempt (praw .>> pzero))` (state test)" <| fun () ->
            let act = fst (run xs (attempt ( attempt (praw .>> pzero))) : Result<int,int,_>)
            Assert.Equal("", (xs,()), act)
        testCase "very big stmt (state test)" <| fun () ->
            let choice xs = Seq.reduce (<|>) xs
            let p : Pars<_,unit,_> = attempt (choice [pzero; praw >>? pzero; pzero; praw >>? pzero] ) <|> pzero

//            |> mapSnd (Either.either (fun x -> Tree.visualize (sprintf "%A") (snd x) ) (sprintf "Right - %A"))
            Assert.Equal("", (xs,()), fst <| run xs (attempt p <|> pzero))
        // testCase "attempt in attempt" <| fun () ->
        //     let pzero s = pzero <?> s
        //     let s = "1"
        //     Assert.Equal("", (xs, Left (false, Tree.singleton <| Back (List.tail xs,s))), (run xs (attempt ( attempt (praw .>> pzero s))) : Result<int,int>))
        // testCase "one st" <| fun () ->
        //     let s = "1"
        //     Assert.Equal("",  ([1], Left (false, Tree.singleton <| Back ([],s) )), (run [1] (attempt ((praw .>> (pzero <?> s)) <|> praw)) : Result<int, int>))
        // testCase "" <| fun () ->
        //     let pzero s = (<?>) pzero s
        //     let choice xs = Seq.reduce (<|>) xs
        //     // let s = Seq.reduce (sprintf "%s%s") ""
        //     // Seq.reduceBack
        //     // sprintf "%c%c"
        //     let p : Pars<char,unit> = attempt (choice [pzero "1"; praw >>? pzero "2"; pzero "3"; praw >>? pzero "4"] ) <|> pzero "5"
        //     run "abcd" (attempt p <|> pzero "6")
        //     |> mapSnd (Either.either (fun x -> Tree.visualize (sprintf "%A") (snd x) ) (sprintf "Right - %A"))
        //     Assert.Equal("", true, true)
    ]
[<Tests>]
let bindLeftTest =
    testList "'.>>' test; need: 'praw', 'pzero'" [
        testCase "right `zero`" <| fun () ->
            Assert.Equal("", ((List.tail xs,()), change ""), run xs (praw .>> pzero))
        testCase "left `zero`" <| fun () ->
            Assert.Equal("", ((xs,()), notChange ""), run xs (pzero .>> praw))
        testCase "both `praw`" <| fun () ->
            let ys = xs |> List.skip 1
            Assert.Equal("", ((List.tail ys,()), List.head xs |> Right), run xs (praw .>> praw))
    ]
[<Tests>]
let sepByTest =
    let sepByTrue p sep = pipe2 p (many (attempt (sep >>. p)) ) (fun hd tl -> hd::tl) <|> (preturn [])
    let pchar c = satisfy ((=) c) "except ','"
    testList "'sepBy' test; need: 'praw', 'satisfy'" [
        testCase "number seq with sep - ','" <| fun () ->
            let xs = "1,2,3"
            Assert.Equal("", (([],()), Right ['1'; '2'; '3']), run xs <| sepBy praw (pchar ',') )
        testCase "number seq with sep - ',' and end sep" <| fun () ->
            let xs = "1,2,3,"
            Assert.Equal(xs, (([','],()), Right ['1'; '2'; '3']), run xs <| sepBy praw (pchar ',') )
        testCase "empty" <| fun () ->
            Assert.Equal("always return empty if p fail", (([],()), Right []), run "" <| sepBy pzero pzero)
        // testCase "attempt" <| fun () ->
        //     ([1..5], 0) |> fun (xs, sep) ->
        //     List.map List.singleton xs
        //     |> List.reduce (fun x y -> x @ [sep] @ y) |> flip List.append [0]
        //     |> fun ys ->
        //     Assert.Equal("", ([sep], Right xs), run ys (sepBy praw (satisfy ((=) 0) null)))
        // testCase "attempt in attempt" <| fun () ->
        //     ([1..5], 0) |> fun (xs, sep) ->
        //     List.map List.singleton xs
        //     |> List.reduce (fun x y -> x @ [sep] @ y) |> List.append [0]
        //     |> fun xs -> (* ([sep], Right [1; 2; 3; 4; 5]) = *) run xs (sepBy praw (satisfy ((=) 0) null))

        //     Assert.Equal("", (List.tail ys, List.head xs |> Right), run xs (praw .>> praw))
    ]

//run [1..100000] (many praw) |> ignore



// [<Tests>]
// let all =
//     testList "all" [
//         runTest
//         preturnTest
//         satisfyTest
//         pzeroTest
//         bindTest
//         bindRightTest
//         orTest
//         manyTest
//         attemptTest
//         bindLeftTest
//         sepByTest
//     ]
//sepByTest |> runParallel
//runParallel all
