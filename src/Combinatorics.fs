namespace FsharpMyExtension.Combinatorics
open FsharpMyExtension
open FsharpMyExtension.Collections

type 'a LazyTreeEmpty = LteNil | Lte of 'a * LazyTreeEmpty<'a> seq
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module LazyTreeEmpty =
    /// распаковать в вид `[[1;2]; [1;3]...]`
    let rec unpack pairs =
        let f = function
            | Lte(x, xs) ->
                if Seq.isEmpty xs then
                    Seq.singleton (Seq.singleton x)
                else
                    Seq.map (fun xs -> seq{yield x; yield! xs}) (unpack xs)
            | LteNil -> Seq.singleton Seq.empty
        Seq.collect f pairs

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

    let rec combAllLazy xs =
        let map fn = Seq.unfold (function [] -> None | h::t -> Some(fn h t, t))
        seq{ yield LteNil
             yield! map (fun x t -> Lte(x, (combAllLazy t))) xs }
    assert
        combAllLazy [1..3]
        |> LazyTreeEmpty.unpack |> Seq.map List.ofSeq |> List.ofSeq
        |> (=) [[]; [1]; [1; 2]; [1; 2; 3]; [1; 3]; [2]; [2; 3]; [3]]
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
    /// Бесконечные сочетания с повторениями:
    ///
    /// `f [1..3]` -&gt; (если отсеять до 2-ух в ширину) `[[1; 1]; [1; 2]; [1; 3]; [2; 2]; [2; 3]; [3; 3]]`
    ///
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
    /// Сочетания с повторениями:
    /// `f 2 [1..3]` -&gt; `[[1; 1]; [1; 2]; [1; 3]; [2; 2]; [2; 3]; [3; 3]]`
    let combRep i xs =
        let rec count i xs =
            if i = 0 then Seq.empty
            else
                xs |> Seq.map (fun (LT(x,xs)) ->
                                LT(x, count (i - 1) xs))
        combRepLazy xs |> count i

    /// Произведение списков:
    /// f `[['1'..'2']; ['a'..'c']]` -&gt; `[['1'; 'a']; ['1'; 'b']; ['1'; 'c']; ['2'; 'a']; ['2'; 'b']; ['2'; 'c']]`
    let pow xss =
        let rec f = function
            | xs::xss -> Seq.map (fun x -> LT(x, f xss)) xs
            | [] -> Seq.empty
        f xss
    /// Перестановка без повторений в алфавитном порядке:
    /// `f [1..3]` -&gt; `[[1; 2; 3]; [1; 3; 2]; [2; 1; 3]; [2; 3; 1]; [3; 1; 2]; [3; 2; 1]]`
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

    /// `f [1;2]` -&gt; `[[1;1]; [1;2]; [2;1]; [1;2]]`
    let permRepLazy xs =
        let rec f () =
            xs |> Seq.map (fun x -> LT(x, f()))
        f ()
module CombUnpacked =
    /// `[1..3]` -&gt; `[[]; [1]; [1; 2]; [1; 2; 3]; [1; 3]; [2]; [2; 3]; [3]]`
    let rec combAll xs =
        let rec map fn xs =
            let rec f acc = function
                | [] -> acc
                | x::xs -> f (fn x xs::acc) xs
            f [] xs |> List.rev
        let f x xs = List.map (fun xs -> x::xs) xs
        [[]] :: map (fun x xs -> f x (combAll xs)) xs
        |> List.concat
    assert
        combAll [] = [[]]
    assert
        let xs = [1..3]
        combAll xs = [[]; [1]; [1; 2]; [1; 2; 3]; [1; 3]; [2]; [2; 3]; [3]]
    let rec combAllLazy xs =
        let map fn = Seq.unfold (function [] -> None | h::t -> Some(fn h t, t))
        let f x xs = Seq.map (fun t -> seq{ yield x; yield! t}) xs
        seq{ yield Seq.singleton Seq.empty
             yield! map (fun x t -> f x (combAllLazy t)) xs }
        |> Seq.concat
    assert
        let test xs = combAllLazy xs |> Seq.map (List.ofSeq) |> List.ofSeq = combAll xs
        test [1..12]
    /// Перестановки без повторений в странном порядке:
    ///
    /// `[1..3]` -> `[[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; [3; 1; 2]; [3; 2; 1]]`
    let permutation xs =
        let rec inserts x = function
            | []           -> [[x]]
            | (y::ys) as l ->
                (x::l)::(List.map (fun xs -> y::xs) (inserts x ys))
        let rec perm = function
            | x :: xs -> Seq.collect (inserts x) (perm xs)
            | []      -> Seq.singleton []
        perm xs
    /// Перестановки с повторениями.
    let permRep k l =
        let add e = function
            | [] -> []
            | l -> List.map (fun x -> e::x) l
        let rec f = function
            | 0 -> List.map (fun x -> [x]) l
            | k -> List.collect (fun x -> add x (f (k-1))) l
        f (k - 1)
module Formulas =
    let inline fact n =
        List.reduce (*) [ LanguagePrimitives.GenericOne .. n ]
    let inline perm n k =
        List.reduce (*) [ k .. n ]
    let inline comb n k = fact n / (fact (n-k) * fact k)
    let inline combRep n k =
        fact(n + k - LanguagePrimitives.GenericOne) / (fact k * fact (n - LanguagePrimitives.GenericOne))
    let pow l =
        l |> List.map (List.length) |> List.filter ((<>) 0)
        |> List.fold (fun (state:System.Numerics.BigInteger) (y:int) ->
            state * bigint y) LanguagePrimitives.GenericOne

/// https://stackoverflow.com/questions/18613690/calculate-nth-multiset-combination-with-repetition-based-only-on-index
module UnionManual =
    (*
    largest[i_, nn_, kk_] := With[
        {x = g[nn, kk]},
        If[x > i, largest[i, nn-1, kk], {nn,x}]
    ]
    *)
    let union n k i j =
        if i > (k - 1) then failwith "j не должно превышать k"

        let fact = function
            | n when n < 0 -> 0
            | 0 -> 1 | 1 -> 1
            | n -> List.reduce (*) [2..n]

        let g n k = fact(n + k - 1) / (fact k * fact (n - 1))

        let l i k =
            if i <= 0 then 0, 0
            else
                let rec f n k =
                    let x = g n k
                    if x > i then f (n-1) k else n, x
                f n k

        let rec f j acc k =
            if k = i then acc
            else
                let v, off = l j k
                f (j-off) v (k-1)
        f j 0 k
