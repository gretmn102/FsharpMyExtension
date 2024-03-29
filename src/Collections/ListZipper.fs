﻿namespace FsharpMyExtension.Collections

type ListZipper<'a> = { Index:int; Left:'a list; Current:'a; Right:'a list }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module ListZipper =
    let singleton x = { Index = 0; Left = []; Current = x; Right = [] }
    let isSingleton x =
        List.isEmpty x.Right && List.isEmpty x.Left
    let ofList = function
        | [] -> failwith "list is empty"
        | h::t -> { Left = t; Current = h; Right = []; Index = 0 }
    let toList { Left = l; Current = h; Right = r } = List.rev r @ h::l
    let toArray { Left = l; Current = h; Right = r } =
        List.rev r @ h::l |> Array.ofList // OPTIMIZE
    let next lst = //(Lz(xs, x, ys)) =
        match lst.Left with
        | h::t ->
            {
                Left = t
                Current = h
                Right = lst.Current::lst.Right
                Index = lst.Index + 1
            } |> Some
        | [] -> None //failwith "left list is empty"
    let prev lst =
        match lst.Right with
        | h::t ->
            {
                Left = lst.Current::lst.Left
                Current = h
                Right = t
                Index = lst.Index - 1 }
            |> Some
        | [] -> None //failwith "right list is empty"

    assert (ofList [1..10] |> next |> Option.get |> next |> Option.get |> toList = [1..10])
    assert
        let xs = ofList [1..10]
        assert (xs = {Left=[2; 3; 4; 5; 6; 7; 8; 9; 10]; Current = 1; Right=[]; Index = 0})

        let next' xs = Option.get <| next xs
        let prev' xs = Option.get <| prev xs

        next' xs |> next' |> next' |> prev' |> prev' |> prev' = xs

    let removeR lz =
        match lz with
        | { Left = h::t; Index = i } ->
            { lz with Left = t; Current = h; Index = i }
            |> Some
        | { Right = h::t; Index = i } ->
            { lz with Right = t; Current = h; Index = i - 1 }
            |> Some
        | { Left = []; Right = [] } -> None
    let removeL lz =
        match lz with
        | { Right = h::t; Index = i } ->
            { lz with Right = t; Current = h; Index = i - 1 }
            |> Some
        | { Left = h::t; Index = i } ->
            { lz with Left = t; Current = h; Index = i }
            |> Some
        | { Left = []; Right = [] } -> None

    ///**Description**
    ///
    ///**Exceptions**
    ///  * `System.ArgumentException`: Thrown when the `Current` does not have precisely one element.
    let concat lst = //(Lz(xs, x, ys)) =
        {
            Left    = List.concat lst.Left
            Current = List.exactlyOne lst.Current
            Right   = List.concat lst.Right
            Index   = lst.Index
        }
    let set x (lz:ListZipper<_>) = { lz with Current = x }
    let hole lst = lst.Current
    let update f (lz:ListZipper<_>) = { lz with Current = f lz.Current }
    let updateFold f (st:'State) (lz:ListZipper<_>) =
        let x, (st:'State) = f st lz.Current
        { lz with Current = x }, st

    let rec toStart lz =
        match prev lz with
        | Some x -> toStart x
        | None -> lz
    let rec toEnd lz =
        match next lz with
        | Some x -> toEnd x
        | None -> lz

    let rec seqf fn n lz =
        if n = 0 then Some lz
        else
            match fn lz with
            | Some lz -> seqf fn (n-1) lz
            | None -> None
    let nexts n lz = seqf next n lz
    let prevs n lz = seqf prev n lz

    assert
        let xs = ofList [1..10]

        assert
            xs |> nexts 6 |> Option.get |> toStart = xs
        assert
            xs |> nexts 6 |> Option.get |> toEnd |> prevs 5 |> Option.get |> toStart = xs
        assert
            xs |> nexts 5 |> Option.get |> removeR = Some {Left=[8; 9; 10];Current=7;Right=[5; 4; 3; 2; 1]; Index = 5}
        true
    let nth n lz =
        let x = lz.Index - n
        if x > 0 then prevs (abs x) lz
        elif x < 0 then nexts (abs x) lz
        else Some lz
    assert
        let xs = { Index = 0;
                   Left = [6; 7; 8; 9];
                   Current = 5;
                   Right = [4; 3; 2; 1];}
        assert
            nth -4 xs = Some { Index = -4;
                               Left = [2; 3; 4; 5; 6; 7; 8; 9];
                               Current = 1;
                               Right = [];}
        assert
            nth 3 xs = Some { Index = 3;
                              Left = [9];
                              Current = 8;
                              Right = [7; 6; 5; 4; 3; 2; 1];}
        nth 5 xs = None
    let iter fn lz =
        List.iter fn lz.Left
        fn lz.Current
        List.iter fn lz.Right
    let map fn lz = {
        Left = List.map fn lz.Left
        Current = fn lz.Current
        Right = List.map fn lz.Right
        Index = lz.Index
        }
    assert
        map ((+) 1) <| ofList [1..10] = {Index = 0; Left = [3..11]; Current = 2; Right = [];}

    let mapi fn lz =
        let currIdx = lz.Index
        {
            Left = List.mapi (fun i x -> fn (i + currIdx + 1) x) lz.Left
            Current = fn currIdx lz.Current
            Right = List.mapi (fun i x -> fn (currIdx - 1 - i) x) lz.Right
            Index = lz.Index
        }

    let mapStartMidEnd start mid fend (lz:_ ListZipper) =
        let firstMap xs =
            let rec loop acc = function
                | [x] ->
                    start false x :: acc |> List.rev
                | x::xs ->
                    loop (mid false x :: acc) xs
                | [] -> List.rev acc
            loop [] xs
        let lastMap xs =
            let rec loop acc = function
                | [x] ->
                    fend false x :: acc |> List.rev
                | x::xs ->
                    loop (mid false x :: acc) xs
                | [] -> List.rev acc
            loop [] xs
        match lz.Left, lz.Right with
        | [], [] ->
            { lz with Current = start true lz.Current }
        | [], right ->
            { lz with
                Current = fend true lz.Current
                Right = firstMap right }
        | left, [] ->
            { lz with
                Left = lastMap left
                Current = start true lz.Current }
        | left, right ->
            { lz with
                Left = lastMap left
                Current = mid true lz.Current
                Right = firstMap right }

    let tryFind fn lz =
        if fn (hole lz) then
            Some lz
        else
            let rec prevs move lz =
                match move lz with
                | Some lz ->
                    if fn (hole lz) then
                        Some lz
                    else
                        prevs move lz
                | None -> None
            prevs prev lz
            |> Option.orElseWith (fun () ->
                prevs next lz
            )

    let insertAfter x (lst:ListZipper<_>) =
        { lst with Current = x
                   Right = lst.Current::lst.Right
                   Index = lst.Index + 1 }
    let insertBefore x (lst:ListZipper<_>) =
        { lst with Current = x
                   Left = lst.Current::lst.Left
                   Index = lst.Index }

    assert
        assert
            insertAfter 2 <| ofList (1::[3..10]) = {Index = 1; Left = [3..10]; Current = 2; Right = [1];}
        insertBefore 0 <| ofList [1..10] = {Index = -1; Left = [1..10]; Current = 0; Right = [];}

    let rep fn n x lz = [1..n] |> List.fold (fun st _ -> fn x st) lz
    let inserterBefore n x lz = rep insertBefore n x lz
    let inserterAfter n x lz = rep insertAfter n x lz
    assert
        inserterBefore 3 -1 <| ofList [1..10] = {Index = -3; Left = [-1; -1] @ [1..10]; Current = -1; Right = [];}

    let insertWhile n x lz =
        let diff = lz.Index - n
        if diff > 0 then inserterBefore (abs diff) x lz
        elif diff < 0 then inserterAfter (abs diff) x lz
        else lz
    assert
        insertWhile 5 -1 <| ofList [1..10] = {Index = 5; Left = [2..10]; Current = -1; Right = [-1; -1; -1; -1; 1];}
    assert
        insertWhile -5 -1 <| ofList [1..10] = {Index = -5; Left = [-1; -1; -1; -1;] @ [1..10]; Current = -1; Right = [];}
    assert
        ofList [1..5] |> toEnd |> insertWhile 8 -1 = {Index = 8; Left = []; Current = -1; Right = [-1; -1; -1;] @ List.rev [1..5];}

//    let nthDef n x lz =
//        let rec toStart lz = match prev lz with Some x -> toStart x | None -> lz
//        let rec toEnd lz = match next lz with Some x -> toEnd x | None -> lz
//        let rec fn n lz =
//            if n = 0 then lz
//            else
//                match next lz with
//                | Some x -> fn <| n-1 <| x
//                | None -> inserterBefore n
//        if n > 0 then toStart lz
//        else toEnd lz

    let nthDef n x lz =
        match nth n lz with
        | Some x -> x
        | None -> (if n > 0 then toEnd else toStart) lz |> insertWhile n x
//    assert
//        nthDef 7 -1 <| ofList [1..5]
//        nthDef 4 -1 <| ofList [1..5]
//        nthDef -1 -1 <| ofList [1..5]
//        true
