module ParserPrimitivesTest

#if INTERACTIVE
#load "ParserPrimitives.fs"
//#load "List.fs"
//#load "Either.fs"
#endif
open FsharpMyExtension.Either
open FsharpMyExtension.List
open FsharpMyExtension.FSharpExt
open FsharpMyExtension.String

open Parser.Primitives

let change s = Left (true, [NotBack s])
let xs = [1..5]
(List.tail xs, List.head xs |> Right) = run xs praw

// preturn
let x = 1
(xs, x |> Right) = run xs (preturn x)

// satisfy
(xs, notChange "some") = run xs (satisfy (k false) "some")
(List.tail xs, List.head xs |> Right) = run xs (satisfy (k true) null)

// pzero
(xs, notChange "") = run xs pzero

// (>>=), preturn, pzero
(xs, notChange "") = run xs ( pzero >>= fun _ -> praw )
(List.tail xs, change "") = (run xs ( praw >>= fun _ -> pzero ) : Result<int,unit>)
xs |> List.skip 1 |> fun ys -> (List.tail ys, (List.head xs, List.head ys) |> Right) = run xs ( praw >>= fun x -> praw >>= fun y -> preturn (x, y) )

// (>>.), praw, pzero
(List.tail xs, change "") = (run xs (praw >>. pzero) : Result<int, int>)
xs |> List.skip 1 |> fun ys -> (List.tail ys, List.head ys |> Right) = run xs (praw >>. praw)

// (<|>), pzero, (>>.)
(xs, Left (false, [NotBack ""; NotBack ""])) = (run xs (pzero <|> pzero) : Result<int,int>)
(List.tail xs, List.head xs |> Right) = run xs (pzero <|> praw)
([], Left (true, [NotBack "1"])) = (run [1] (((praw .>> (pzero <?> "1")) <|> praw)) : Result<int, int>)

// many, satisfy, pzero
let n = 3
let ys = List.take n xs
(List.skip n xs, Right ys) = run xs (many (satisfy (Set.ofList ys |> flip Set.contains) null))
(xs, Right []) = run xs (many pzero)

let rec manyTrue p = (p >>= fun x -> manyTrue p |>> fun xs -> x::xs ) <|> preturn []

(fun (n:int) -> satisfy ((=) n) (sprintf "%d" n))
|> fun pdigit ->
    let p f = run [1;3] (f (pdigit 1 >>. pdigit 2 )) // ->  ([3], Left (true, [NotBack "2"]))
    p manyTrue = p many

// attempt, praw, pzero, (>>.)
(xs, Left (false, [Back (List.tail xs,"")])) = (run xs (attempt (praw >>. pzero)) : Result<int,int>)
(xs, Left (false, [Back (List.tail xs,"")])) = (run xs (attempt ( attempt (praw .>> pzero) >>. praw)) : Result<int,int>)
([1], Left (false, [Back ([],"1")])) = (run [1] (attempt ((praw .>> (pzero <?> "1")) <|> praw)) : Result<int, int>)

// (.>>), praw, pzero
(List.tail xs, change "") = run xs (praw .>> pzero)
xs |> List.skip 1 |> fun ys -> (List.tail ys, List.head xs |> Right) = run xs (praw .>> praw)

// sepBy, praw, satisfy
([1..5], 0) |> fun (xs, sep) ->
    List.map List.singleton xs
    |> List.reduce (fun x y -> x @ [sep] @ y) |> flip List.append [0]
    |> fun ys -> ([sep], Right xs) = run ys (sepBy praw (satisfy ((=) 0) null))
([1..5], 0) |> fun (xs, sep) ->
    List.map List.singleton xs
    |> List.reduce (fun x y -> x @ [sep] @ y) |> List.append [0]
    [0;0;]
    |> fun xs -> (* ([sep], Right [1; 2; 3; 4; 5]) = *) run xs (sepBy praw (satisfy ((=) 0) null))

module FParsec =
    open FParsec.Primitives
    open FParsec.CharParsers
    //let x = run (anyChar >>. (pzero <?> "1" <|> (pzero <?> "2") <|> (pzero <?> "3") )) "12" : ParserResult<unit,unit>
    let pzero s = pzero <?> s
    let x : Parser<unit, unit> = attempt (choice [pzero "1"; anyChar >>? pzero "2"; pzero "3"; anyChar >>? pzero "4"] ) <|> pzero "5"
    run x "12"
    run (x <|> (anyChar >>. pzero "6")) "12"
    run (anyChar <?> "" ) "12"
// 

run "a" (satisfy (k false) null)
let pchar c = satisfy ((=) c) (sprintf "expected char '%c'" c)
run "abc" (attempt (pchar 'a' >>. pchar 'c'))
|> function ['a'; 'b'; 'c'], Left _ -> true | _ -> false
run "abc" (pchar 'a' >>. pchar 'c')
|> function ['b'; 'c'], Left _ -> true | _ -> false
run "abc" (pchar 'd' <|> pchar 'e' <|> (attempt (pchar 'a' >>. (pchar 'a' <|> pchar 'c'))) <|> pchar 'b')