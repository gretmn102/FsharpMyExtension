namespace FsharpMyExtension.List

[<RequireQualifiedAccess>]
module List =
    open FsharpMyExtension.FSharpExt
    let cons x xs = x::xs
    let consFlip xs x = x::xs
    
    [<System.ObsoleteAttribute("use 'List.splitAt' in Fsharp.Core 4.0")>]
    let trunc n xs = 
        let rec f n acc = function
            | [] -> acc, []
            | h::t as xs ->
                if n = 0 then acc, xs
                else f (n - 1) (h::acc) t
        if n < 0 then failwith "n must be greater 0"
        let (h, t) = f n [] xs in List.rev h, t
    assert
        trunc 5 [1..10] = ([1..5], [6..10])
    assert
        trunc 5 [1..4] = ([1..4], [])
    assert
        trunc 0 [1..10] = ([], [1..10])
    
    /// takeWhile = truncWhile ?
    [<System.ObsoleteAttribute("use 'takeWhileRest'")>]
    let truncWhile p xs = 
        failwith "use 'takeWhileRest'"
    //     let rec f acc = function
    //         | [] -> acc, []
    //         | h::t as xs ->
    //             if p h then f (h::acc) t
    //             else acc, xs
    //     let (h, t) = f [] xs in List.rev h, t
    // assert
    //     truncWhile (fun x -> x % 2 = 0) [2; 4; 6; 7; 8; 9] = ([2;4;6], [7; 8; 9])
    [<System.ObsoleteAttribute("use 'takeWhileRest'")>]
    /// takeWhile = truncWhile ?
    let takeWhile p =
        failwith "use 'takeWhileRest'"
    //     let rec f acc = function
    //         | [] -> List.rev acc, []
    //         | h::t as xs ->
    //             if p h then f <| h::acc <| t
    //             else List.rev acc, xs
    //     f []
    // assert
    //     takeWhile (fun x -> x % 2 = 0) [2; 4; 6; 7; 8; 9] = ([2;4;6], [7; 8; 9])
    // assert
    //     takeWhile (flip (%) 2 >> (=) 0) [2;4;5;6] = ([2;4], [5;6])
    // assert
    //     takeWhile (konst id true) [] = ([],[])
    // assert
    //     takeWhile (flip (%) 2 >> (=) 0) [3;4;5;6] = ([], [3;4;5;6])

    ///**Description**
    /// * takeWhileRest (fun x -> x % 2 = 0) [2; 4; 6; 1; 2; 4] = ([2;4;6], [7;8;9])
    ///**Parameters**
    ///  * `p` - parameter of type `'a -> bool`
    ///
    ///**Output Type**
    ///  * `'a list -> 'a list * 'a list`
    ///
    ///**Exceptions**
    /// ?
    let takeWhileRest p =
        let takeWhileRest p =
            let rec f acc = function
                | [] -> List.rev acc, []
                | h::t as xs ->
                    if p h then f <| h::acc <| t
                    else List.rev acc, xs
            f []
        /// faster then `takeWhileRest`?
        let takeWhileRest2 pred =
            let finished = ref true
            List.partition (pred >> fun r -> !finished && (finished := r; r))
        // assert
        //     let sw f =
        //         let s = System.Diagnostics.Stopwatch()
        //         s.Start()
        //         f() |> ignore
        //         s.Stop()
        //         s.Elapsed

        //     let p = ((>) (10000000 / 3))
        //     let xs = [1..10000000]
        //     takeWhileRest p xs = takeWhileRest2 p xs
        //     ((fun () -> takeWhileRest p xs) |> sw) - ((fun () -> takeWhileRest2 p xs) |> sw)
            
        //     true
        takeWhileRest2 p
    assert
        takeWhileRest (fun x -> x % 2 = 0) [2; 4; 6; 1; 2; 4] = ([2; 4; 6], [1; 2; 4])
    

    ///**Description**
    /// * `truncList` 2 [1..5] -> [[1; 2]; [3; 4]; [5]]
    /// * `truncList` 3 [1..8] -> [[1; 2; 3]; [4; 5; 6]; [7; 8]]
    ///**Parameters**
    ///  * `n` - parameter of type `int`
    ///  * `xs` - parameter of type `'a list`
    ///
    ///**Output Type**
    ///  * `'a list list`
    [<System.ObsoleteAttribute("use 'List.chunkBySize' in Fsharp.Core 4.0, and 'n' must be >0")>]
    let truncList n xs = 
        let rec f acc = function
            | [] -> acc
            | xs -> let (h, t) = trunc n xs in f (h::acc) t
        if n = 0 then [xs]
        else f [] xs |> List.rev
    assert
        truncList 3 [1..10] = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]; [10]]
    assert
        truncList 3 [1..9] = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9];]
    assert
        truncList 5 [1..4] = [[1..4]]
    assert
        truncList 0 [1..10] = [[1..10]]
    assert
        List.allPairs [1..10] [[1..5]; [1..10]]
        |> List.tryPick (fun ((n, xs) as x) ->
            (curry List.chunkBySize x, curry truncList x) |> cond (curry (<>)) (comma x >> Some) (k None))
        |> Option.isNone

    ///**Description**
    /// * transpose lists
    /// [[Some 1; Some 1;  Some 1]
    /// [Some 2; Some 2;  Some 2]
    /// [Some 3; Some 3;  Some 3]
    /// [None;   Some 4;  Some 4]
    /// [None;   Some 5;  Some 5]
    /// [None;   Some 6;  None  ]
    /// [None;   Some 7;  None  ]
    /// [None;   Some 8;  None  ]
    /// [None;   Some 9;  None  ]
    /// [None;   Some 10; None  ]]
    ///**Parameters**
    ///  * `xss` - parameter of type `'a list list`
    ///
    ///**Output Type**
    ///  * `'a option list list`
    let transposeOpt xss =
        let ss xss =
            List.foldBack (fun x (xs, yss) -> match x with (h::t) -> Some h::xs, t::yss | [] -> None::xs, []::yss) xss ([], [])
        ss xss |> List.unfold (function xs, _ when List.forall Option.isNone xs -> None | (xs, yss) -> Some(xs, ss yss))    
    assert
        let input = 
            [[1; 2; 3];
             [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];
             [1; 2; 3; 4; 5]]
        let expepected = 
            [[Some 1; Some 1;  Some 1]
             [Some 2; Some 2;  Some 2]
             [Some 3; Some 3;  Some 3]
             [None;   Some 4;  Some 4]
             [None;   Some 5;  Some 5]
             [None;   Some 6;  None  ]
             [None;   Some 7;  None  ]
             [None;   Some 8;  None  ]
             [None;   Some 9;  None  ]
             [None;   Some 10; None  ]]

        let actual = transposeOpt input
        actual = expepected
    [<System.ObsoleteAttribute("use 'transposeOpt'")>]

    let transpose (L:'a list list) : ('a option list list) = 
        failwith "use 'transposeOpt'"
        // let f1 x = List.map (function | hd::tl -> Some hd, tl | [] -> None, []) x
    
        // let f2 x = 
        //     List.foldBack (fun x (ls, Ls) ->
        //                         match x with
        //                         | Some a, l -> Some a :: ls, l :: Ls
        //                         | None, l -> None :: ls, l :: Ls) x ([], [])
        // let res = ref []
        // let f3 (l, ls) = res := l :: !res; f1 ls
    
        // let l1 = f1 L
        // let rec f l = 
        //     if l |> List.exists (function _, x -> not (List.isEmpty x)) then f (f3 (f2 l)) else l
        // in f l1 |> ignore
        // !res |> List.rev

    ///**Description**
    ///  * transpose matrix
    ///  * `[[1; 2; 3]; [4; 5; 6]; [7; 8; 9]]` -> `[[1; 4; 7]; [2; 5; 8]; [3; 6; 9]]`
    ///  * `[[1; 2]; [3; 4]; [5; 6]]` -> `[[1; 3; 5]; [2; 4; 6]]`
    ///**Parameters**
    ///  * `xss` - parameter of type `'a list list`
    ///**Output Type**
    ///  * `'a list list`
    ///**Exceptions**
    ///  * System.ArgumentException: Throw then the lists tail of `xss` has different lengths.
    ///  * trans [[1;2]; [3;4;5]] -> [[1; 3]; [2; 4]]
    ///  * but trans [[1;2]; [3]] -> Exception
    let trans xss = 
        let ss = function 
            | []::_ -> [], []
            | xss -> List.foldBack (fun x (xs, yss) -> match x with (h::t) -> h::xs, t::yss | _ -> raise (System.ArgumentException("The lists had different lengths")) ) xss ([], [])
        ss xss |> List.unfold (function [], _ -> None | (xs, yss) -> Some(xs, ss yss))
    assert
        let xss = List.chunkBySize 2 [1..6]
        trans xss = [[1; 2]; [3; 4]; [5; 6]]
    
    [<System.ObsoleteAttribute("use 'transposeOpt'")>]
    let trans2 xss = failwith "use 'transposeOpt'"
    let shuffle cards =
        let rnd = new System.Random()
        let ranOf (l:System.Collections.Generic.List<'a>) =
            let n = Seq.length l
            let r = rnd.Next(0, n)
            l.[r]
        let s = new System.Collections.Generic.List<'a>(Seq.ofList cards)
        let rec f acc =
            if s.Count <> 0 then
                let current = ranOf s
                s.Remove current |> ignore
                f (current::acc)
            else acc
        f []

    // [<System.ObsoleteAttribute("use 'List.unfold' in Fsharp.Core 4.0")>]
    // let unfold fn (ini:'State) = Seq.unfold fn ini |> List.ofSeq

    let travOpt xs =
        let rec f acc = function
            | x :: t -> x |> Option.bind (fun x -> f <| x :: acc <| t)
            | [] -> Some <| List.rev acc
        f [] xs
    assert
        List.init 10 Some |> travOpt = Some [0..9]
    assert
        [ yield Some 1; yield None; yield Some 3 ] |> travOpt = None


// module T =
//     (*type List<'a> =
//         | Cons of 'a * List<'a>
//         | Nil
//     let (>!) x xs = Cons(x, xs) *)
//     let rec foldBack f st = function
//         | x :: xs -> foldBack f (f x st) xs
//         | [] -> st
    
    
//     //((a + b) + c)
//     List.rev [1..10] |> foldBack (fun x st -> x::st) []
//     let rec fold f st = function
//         | x::xs -> f x (lazy(fold f st xs))
//         | [] -> st
//     let rec fold' fn st =
//         let rec f st = function
//             | x::xs -> fn x  (lazy(f st xs))
//             | [] -> st
//         f st
//     let i = ref 0
//     let fn step ini (xs:'T list) n = fold step ini xs n
//     let take n xs = fn (fun x st -> function 0 -> [] | n -> x :: (st.Value (n - 1))) (fun _ -> []) xs n
//     take 20000 [1..20000]
//     Seq.unfold (fun (xs, ys) -> ys |> function h::t -> Some((h, t), t) | [] -> None ) ([], [1..10])
    
//     i

//     let s = ""