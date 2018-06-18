namespace FsharpMyExtension.Combinatorics
open FsharpMyExtension
module Comb =
    /// Сочетания без повторений:
    /// `f 2 [1..3]` -> `[[1; 2]; [1; 3]; [2; 3]]`
    let rec comb count = 
        if count <= 0 then fun _ -> Seq.empty
        else
            let fn = function
                | h::t as xs ->
                    if List.length xs < count then None
                    else Some(LT(h, comb (count - 1) t), t)
                | [] -> None
            Seq.unfold fn
    assert
        comb 0 [0..4] |> Seq.isEmpty
    assert
        let xs = [1..4]
        comb 1 xs |> List.ofSeq = List.map (fun x -> LT(x, Seq.empty)) xs
    assert
        comb 2 [0..3] |> LazyTree.unpack |> List.ofSeq = [[0; 1]; [0; 2]; [0; 3]; [1; 2]; [1; 3]; [2; 3]]
    assert
        comb 4 [0..3] |> LazyTree.unpack |> List.ofSeq = [[0..3]]
    assert
        comb 5 [0..3] |> Seq.isEmpty

//    let rec permm xs =
//        let fn = function
//            | [] -> None
//            | h::t as xs -> Some(LT(h, seq{ yield Nil; yield! permm t}), t)
//        Seq.unfold fn xs
//    assert
//        
//        permm [0..3] |> LazyTree.unpack
//    let xs = Seq.init 10 (printfn "%d")
//    Seq.take 2 xs
//    Seq.skip 2 xs
    /// Бесконечные сочитания с повторениями:
    /// `f [1..3]` -> (если отсеять до 2-ух в ширину) `[[1; 1]; [1; 2]; [1; 3]; [2; 2]; [2; 3]; [3; 3]]`
    /// Бесконечная как в ширину, так и в длину.
    let combRepLazy = 
        let rec f = function
            | x::xs as ys -> 
                seq {
                    yield LT(x, f ys)
                    yield! f xs
                }
            | [] -> Seq.empty
        f
    /// Сочитания с повторениями:
    /// `f 2 [1..3]` -> `[[1; 1]; [1; 2]; [1; 3]; [2; 2]; [2; 3]; [3; 3]]`
    let combRep i xs = 
        let rec count i xs =
            if i = 0 then Seq.empty
            else
                xs |> Seq.map (fun (LT(x,xs)) ->
                                LT(x, count (i - 1) xs))
        combRepLazy xs |> count i

    /// Произведение списков:
    /// f `[['1'..'2']; ['a'..'c']]` -> `[['1'; 'a']; ['1'; 'b']; ['1'; 'c']; ['2'; 'a']; ['2'; 'b']; ['2'; 'c']]`
    let pow xss =
        let rec f = function
            | xs::xss -> Seq.map (fun x -> LT(x, f xss)) xs
            | [] -> Seq.empty
        f xss
    /// Перестановка без повторений в алфавитном порядке:
    /// `f [1..3]` -> `[[1; 2; 3]; [1; 3; 2]; [2; 1; 3]; [2; 3; 1]; [3; 1; 2]; [3; 2; 1]]`
    let rec permutation xs = 
        // более понятный вариант:
        // /// 1234 -> [1, 234; 2, 134; 3, 124; 4, 123]
        // /// [(1, [2; 3; 4]); (2, [1; 3; 4]); (3, [1; 2; 4]); (4, [1; 2; 3])]
        // let rem xs =
        //     let add xs ys = List.fold (fun st x -> x::st) ys xs
        //     let rec f left = function
        //         | x::xs ->
        //             (x, add left xs) :: f (x::left) xs
        //         | [] -> []
        //     f [] xs
        // let rec f = function
        //     | [] -> Seq.empty
        //     | xs ->
        //         rem xs |> Seq.map (fun (x, xs) -> LT(x, f xs))
        // f xs
        let add xs ys = List.fold (fun st x -> x::st) ys xs
        let rec f left = function
            | x::xs ->
                printfn "%A" (x::xs)
                seq{
                    yield LT(x, permutation (add left xs))
                    yield! f (x::left) xs
                }
            | [] -> Seq.empty
        f [] xs
    // let xs = permutation [1..5]
    // Seq.truncate 4 xs |> List.ofSeq |> ignore
    // // assert
    // let xs = [1..3]
    // // mapBoth LazyTree.unpack (perm3 xs, perm4 xs) |> curry (=)
    // permutation xs |> LazyTree.unpack
    
module CombUnpacked = 
    // open FsharpMyExtension.FSharpExt
    /// Перестановки без повторений в странном порядке:
    /// `[1..3]` -> `[[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; [3; 1; 2]; [3; 2; 1]]`
    let permutation xs = 
        let rec inserts x = function
            | []           -> [[x]]
            | (y::ys) as l ->
                (x::l)::(List.map (fun xs -> y::xs) (inserts x ys))
        // inserts 1 [2..3]
        let rec perm = function
            | x :: xs -> Seq.collect (inserts x) (perm xs)
            | []      -> Seq.singleton []
        perm xs