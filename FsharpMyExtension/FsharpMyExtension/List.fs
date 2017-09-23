namespace FsharpMyExtension.List

[<RequireQualifiedAccess>]
module List =
    let cons x xs = x::xs
    let consFlip xs x = x::xs
    
    [<System.ObsoleteAttribute("use 'List.splitAt'")>]
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

    let truncWhile p xs = 
        let rec f acc = function
            | [] -> acc, []
            | h::t as xs ->
                if p h then f (h::acc) t
                else acc, xs
        let (h, t) = f [] xs in List.rev h, t
    assert
        truncWhile (fun x -> x % 2 = 0) [2; 4; 6; 7; 8; 9] = ([2;4;6], [7; 8; 9])
    
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

    let transpose L = 
        let f1 x = List.map (function | hd::tl -> Some hd, tl | [] -> None, []) x
    
        let f2 x = 
            List.foldBack (fun x (ls, Ls) ->
                                match x with
                                | Some a, l -> Some a :: ls, l :: Ls
                                | None, l -> None :: ls, l :: Ls) x ([], [])
        let res = ref []
        let f3 (l, ls) = res := l :: !res; f1 ls
    
        let l1 = f1 L
        let rec f l = 
            if l |> List.exists (function _, x -> List.length x <> 0) then f (f3 (f2 l)) else l
        in f l1 |> ignore
        !res |> List.rev
            (*
        let l2 = f2 l1
        let l3 = f3 l2
        let l4 = f2 l3
        let l5 = f3 l4
        let l6 = f2 l5
        let l7 = f3 l6
        let l8 = f2 l7
        let l9 = f3 l8
        let l10 = f2 l9
        //!res |> List.rev
        *)

    open FsharpMyExtension.FSharpExt
    let trans xss = 
        let ss = function 
            | []::_ -> [], []
            | xss -> List.foldBack (fun x (xs, yss) -> match x with (h::t) -> h::xs, t::yss | _ -> failwith "The lists had different lengths") xss ([], [])
        ss xss |> Seq.unfold (function [], _ -> None | (xs, yss) -> Some(xs, ss yss)) |> List.ofSeq
//    let rec trans = function
//        | [xs] -> xs |> List.map (fun x -> [x])
//        | xs::xss -> List.map2 (fun x y -> x::y) xs (trans xss)
//        | _ -> failwith "в списке пустой список"
    assert
        let xss = truncList 3 [1..9]
        let s () = 
            let f f ini = List.fold (fun st x ini -> f x st ini) ini [1..3]
            let f f x0 x1 x2 s = fun ini -> f x0 (fun ini -> f x1 (fun ini -> f x2 s ini) ini) ini
            let f f s = fun ini -> f 1 (fun ini -> f 2 (fun ini -> f 3 s ini) ini) ini
            let ss = (fun ini -> (fun x f (i, xs) -> if i > 0 then f(i-1, x::xs) else xs) 1 (fun ini -> (fun x f (i, xs) -> if i > 0 then f(i-1, x::xs) else xs) 2 (fun ini -> (fun x f (i, xs) -> if i > 0 then f(i-1, x::xs) else xs) 3 snd ini) ini) ini) (2, [])
            let ss = if 2 > 0 then (fun x f (i, xs) -> if i > 0 then f(i-1, x::xs) else xs) 2 (fun ini -> (fun x f (i, xs) -> if i > 0 then f(i-1, x::xs) else xs) 3 snd ini) (2-1, 1::[]) else []
            let fold xs = Seq.unfold (function h::t -> Some((h,t), t) | [] -> None) xs |> List.ofSeq
            
            let f fn ini = List.foldBack (fun st x ini -> fn st x ini) <| fold [1..10] <| ini
            let s = f (fun x f (i, xs) -> match x with (_, [_]) -> printfn "sdf"; begin if i then f(true, x::xs) else f(false, x::xs) end | _ -> f(i,x::xs)) id (true, [])
            let ini = (2,[])
            let next = fun x f (i, xs) -> if i > 0 then f(i-1, x::xs) else xs
            
            //(2, []) |> f (fun x f (i, xs) -> if i > 0 then f(i-1, x::xs) else xs) snd
            ()
        trans xss = [[1; 4; 7]; [2; 5; 8]; [3; 6; 9]]
    
    let trans2 xss = 
        let ss xss = List.foldBack (fun x (xs, yss) -> match x with (h::t) -> Some h::xs, t::yss | [] -> None::xs, []::yss) xss ([], [])
        ss xss |> Seq.unfold (function xs, _ when List.forall Option.isNone xs -> None | (xs, yss) -> Some(xs, ss yss)) |> List.ofSeq

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
        let actual = trans2 input |> List.ofSeq
        actual = expepected
    
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

    let takeWhile p =
        let rec takeFirst acc = function
            | [] -> List.rev acc, []
            | h::t as xs -> if p h then takeFirst <| h::acc <| t else List.rev acc, xs
        takeFirst []
    assert
        takeWhile (flip (%) 2 >> (=) 0) [2;4;5;6] = ([2;4], [5;6])
    assert
        takeWhile (konst id true) [] = ([],[])
    assert
        takeWhile (flip (%) 2 >> (=) 0) [3;4;5;6] = ([], [3;4;5;6])

    let unfold fn (ini:'State) = Seq.unfold fn ini |> List.ofSeq

    let travOpt xs =
        let rec f acc = function
            | x :: t -> x |> Option.bind (fun x -> f <| x :: acc <| t)
            | [] -> Some <| List.rev acc
        f [] xs
    assert
        List.init 10 Some |> travOpt = Some [0..9]
    assert
        [ yield Some 1; yield None; yield Some 3 ] |> travOpt = None


