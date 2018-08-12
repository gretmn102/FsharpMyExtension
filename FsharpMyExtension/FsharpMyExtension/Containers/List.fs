namespace FsharpMyExtension.List

[<RequireQualifiedAccess>]
module List =
    open FsharpMyExtension
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
    // assert
    //     trunc 5 [1..10] = ([1..5], [6..10])
    // assert
    //     trunc 5 [1..4] = ([1..4], [])
    // assert
    //     trunc 0 [1..10] = ([], [1..10])
    
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
    /// * `takeWhileRest (fun x -> x % 2 = 0) [2; 4; 6; 1; 2; 4] = ([2;4;6], [1;2;4])`
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
                | h::t as xs ->
                    if p h then f (h::acc) t
                    else List.rev acc, xs
                | [] -> List.rev acc, []
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
            | xs -> let (h, t) = List.splitAt n xs in f (h::acc) t
        if n = 0 then [xs]
        else f [] xs |> List.rev
    // assert
    //     truncList 3 [1..10] = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]; [10]]
    // assert
    //     truncList 3 [1..9] = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9];]
    // assert
    //     truncList 5 [1..4] = [[1..4]]
    // assert
    //     truncList 0 [1..10] = [[1..10]]
    // assert
    //     List.allPairs [1..10] [[1..5]; [1..10]]
    //     |> List.tryPick (fun ((n, xs) as x) ->
    //         (curry List.chunkBySize x, curry truncList x) |> cond (curry (<>)) (comma x >> Some) (k None))
    //     |> Option.isNone

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
        let f xss =
            List.foldBack (fun x (xs, yss) ->
                    match x with
                    | h::t -> Some h::xs, t::yss
                    | [] -> None::xs, []::yss)
                xss ([], [])
        f xss
        |> List.unfold (function
            | xs, _ when List.forall Option.isNone xs -> None
            | xs, yss -> Some(xs, f yss))
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
        let f = function 
            | []::_ -> [], []
            | xss ->
                List.foldBack (fun x (xs, yss) ->
                    match x with
                    | h::t -> h::xs, t::yss
                    | _ ->
                        raise (System.ArgumentException("The lists had different lengths")) )
                    xss
                    ([], [])
        f xss |> List.unfold (function [], _ -> None | xs, yss -> Some(xs, f yss))
    assert
        let xss = List.chunkBySize 2 [1..6]
        trans xss = [[1; 2]; [3; 4]; [5; 6]]
    /// попроще чем `trans`
    let rec trans3 = function
        | []::_ -> []
        | xss ->
            List.map List.head xss :: trans3 (List.map List.tail xss)
    [<System.ObsoleteAttribute("use 'transposeOpt'")>]
    let trans2 xss = failwith "use 'transposeOpt'"
    // /// медленное перемешивание
    // let shuffle xs =
    //     let r = System.Random()
    //     let rec f acc = function
    //         | [] -> acc
    //         | xs -> 
    //             let l = List.length xs
    //             let curr = r.Next(0, l)
    //             let rec f' i acc = function
    //                 | h::t -> 
    //                     if i = curr then h, List.append (List.rev acc) t
    //                     else f' (i+1) (h::acc) t
    //                 | [] -> failwith "empty lst"
    //             let curr, rest = f' 0 [] xs
    //             f (curr::acc) rest
    //     f [] xs |> List.rev
    let shuffle cards =
        let rnd = System.Random()
        let ranOf (l:System.Collections.Generic.List<'a>) =
            let n = Seq.length l
            let r = rnd.Next(0, n)
            l.[r]
        let s = new System.Collections.Generic.List<'a>(Seq.ofList cards)
        let rec f acc =
            if s.Count = 0 then acc
            else
                let current = ranOf s
                s.Remove current |> ignore
                f (current::acc)
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
    ///**Description**
    ///  * Последовательная группировка.
    ///  * `f (=) [1;2;1;1;3;3;4;4;5]` -> `[[1]; [2]; [1; 1]; [3; 3]; [4; 4]; [5]]`
    let groupBySeq f = function
        | x::xs ->
            List.fold (fun (prev, xs) x ->
                match xs with
                | h::t -> 
                    if f prev x then x, (x::h)::t
                    else x, [x]::h::t
                | [] -> failwith "")
                (x, [[x]]) xs
            |> snd |> List.map List.rev |> List.rev
        | [] -> []
    // assert
    //     [] = groupBySeq (=) []
    // assert
    //     [[1]] = groupBySeq (=) [1]
    // assert
    //     groupBySeq (=) [1;2;1;1;3;3;4;4;5] = [[1]; [2]; [1; 1]; [3; 3]; [4; 4]; [5]]
    // assert
    //     let xs = [1,1; 2,1; 3,1]
    //     groupBySeq (fun x y -> snd x = snd y) xs = [xs]
    // assert
    //     groupBySeq (=) [1; 1; 2;3;3] = [[1; 1]; [2]; [3; 3]]
    // assert
    //     groupBySeq (fun x y -> snd x = snd y) [ 1,1; 2,1; 3,2; 4,2 ] = [ [1,1; 2,1;]; [3,2; 4,2] ]
    module Alt =
        /// `f isEven [2;4;5;6;7]` -> `([2; 4], [5; 6; 7])`
        let span fn xs =
            let rec f = function
                | x::xs' as xs ->
                    if fn x then
                        let ys, zs = f xs'
                        x::ys, zs
                    else [], xs
                | xs -> xs, xs
            f xs
        // clear, but not safe
        let groupBySeq2 fn xs =
            let rec f acc = function
                | x::xs ->
                    let xs, ys = span (fn x) xs
                    f ((x::xs)::acc) ys
                | [] -> acc
            f [] xs |> List.rev
        let rec groupBy eq = function
            | x::xs ->
                let (ys, zs) = span (eq x) xs
                (x::ys) :: groupBy eq zs
            | [] -> []
        // span (fun x -> true) [1..2000]
        // let xs = [1,1; 2,1; 3,1]
        
        // groupBy (fun x y -> snd x = snd y) xs = [xs]
        let timer f = 
            let s = System.Diagnostics.Stopwatch()
            s.Start()
            f() |> ignore
            s.Stop()
            s.Elapsed
        let f() = 
            let xs = List.init 10000 (List.replicate 5000)
            timer (fun () -> xs |> groupBySeq2 (=)), timer (fun () -> xs |> groupBySeq (=))

    
    // let xs = List.init 5000 (List.replicate 2)
    // xs |> groupBySeq2 (=)
    // xs |> groupBySeq (=)
    let rec fold' f st = function
        | x::xs -> f x (lazy (fold' f st xs))
        | [] -> st
    ///**Description**
    /// [1..3] -> [1;2;3;1;2;3;1...]
    ///**Parameters**
    ///  * `xs` - parameter of type `list<'a>`
    /// Why not `seq` in input? Because `seq` - potential infinity structure and
    /// it `list` cache structure.
    ///**Output Type**
    ///  * `seq<'a>`
    ///**Exceptions**
    /// System.ArgumentException: Thrown when the list is empty.
    let circle (xs: _ list) =
        if List.isEmpty xs then raise (System.ArgumentException "The input list was empty.")
        seq { while true do for x in xs do yield x }



    ///**Description**
    /// Цель: преобразовать элементы в строковое значение так, чтобы
    /// после сортировки полученных строковых значений, последовательность
    /// была такой же как у исходного списка.
    /// Решение: в начале каждого элемента нумерацию в стиле:
    /// "01, 02,..., 10, 11,..." и прибавить к этому по-умолчанию `sprintf "_A"`.
    ///**Parameters**
    ///  * `fn` - parameter of type `('a -> string) option`
    ///  * `xs` - parameter of type `seq<'a>`
    ///**Output Type**
    ///  * `seq<'a * string>`
    let numerate fn xs =
        let print = defaultArg fn (sprintf "_%A")
        let capacityNum = 
            List.length xs - 1 |> fun x -> x.ToString() |> String.length
        xs
        |> List.mapi (fun i num ->
            let str = i.ToString()
            let str =
                sprintf "%s%s%s"
                    (String.replicate (capacityNum - str.Length) "0")
                    str
                    (print num)
            num, str)

    /// `List.splitInto size [0..length - 1] |> List.map (on List.head List.last)`
    let inline splitIntoRange size length =
        let size = int size
        let len = int (float length / float size)
        let n = length - size * len
        let xs = List.replicate n (succ len) @ List.replicate (size - n) len
        List.tail xs
        |> List.scan
            (fun (_,i) x -> succ i, i + x)
            (LanguagePrimitives.GenericZero, pred (List.head xs))
    assert
        let size, length = 41, 1234
        List.splitInto size [0..length - 1] |> List.map (on List.head List.last) = splitIntoRange size length
    /// `List.splitInto size [0..length - 1] |> List.map (on List.head List.last)`
    let inline splitIntoRange64 size length =
        let size = int64 size
        let len = int64 (float length / float size)
        let n = length - size * len
        let xs = List.replicate (int n) (succ len) @ List.replicate (int (size - n)) len
        List.tail xs
        |> List.scan
            (fun (_,i) x -> succ i, i + x)
            (LanguagePrimitives.GenericZero, pred (List.head xs))
    /// `List.chunkBySize size [0..length - 1] |> List.map (on List.head List.last)`
    let inline chunkBySizeRange size length =
        // let length, parts = 9, 5
        let parts = int size
        let len = int (float length / float parts)
        let n = length - parts * len
        let xs = List.replicate len parts @ [n]

        // List.chunkBySize parts [0..length - 1] |> List.map List.length

        List.tail xs
        |> List.scan
            (fun (_,i) x -> succ i, i + x)
            (LanguagePrimitives.GenericZero, pred (List.head xs))
    assert
        let length, size = 9, 5
        List.chunkBySize size [0..length - 1] |> List.map (on List.head List.last) = chunkBySizeRange size length