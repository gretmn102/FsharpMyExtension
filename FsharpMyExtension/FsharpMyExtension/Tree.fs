module FsharpMyExtension.Tree
open FsharpMyExtension.FSharpExt
type Tree<'a> = Node of 'a * Tree<'a> list
module Tree =
    let singleton x = Node(x, [])
    let visualize print inputTree =
        let prefMid = seq { yield "├─"; while true do yield "│ " }
        let prefEnd = seq { yield "└─"; while true do yield "  " }
        let prefNone = seq { while true do yield "" }

        let inline c2 x y = Seq.map2 (+) x y

        let rec visualize (Node(label:'a, children:Tree<'a> list)) pre =
            seq {
                yield Seq.head pre + print label
                match children with
                | [] -> ()
                | children ->
                    let preRest = Seq.skip 1 pre
                    let fn = function
                        | [e] -> Some(visualize e (c2 preRest prefEnd), [])
                        | e::t -> Some(visualize e (c2 preRest prefMid), t)
                        | [] -> None

                    yield! Seq.unfold fn children |> Seq.concat
            }
        System.String.Join("\n", visualize inputTree prefNone)

    assert
        let dummy =
            Node ("root",
                    [Node ("a",
                            [Node ("a1",
                                    [Node ("a11", []);
                                     Node ("a12", []) ]) ]);
                    Node ("b",
                            [Node ("b1", []) ]) ])
        visualize (sprintf "%s") dummy = "root\n├─a\n│ └─a1\n│   ├─a11\n│   └─a12\n└─b\n  └─b1"
    let getValue (Node(x, _)) = x
    let getBranch (Node(_, x)) = x
    let leafs inputTree =
        let rec f acc = function
            | Node(x, []) -> x::acc
            | Node(x, xs) ->
                List.foldBack (fun x state -> f state x) xs acc
        f [] inputTree

    assert
        let tr = Node ("b",[Node ("c",[Node ("d",[])]); Node ("d",[Node ("a",[]); Node ("c",[])]); Node ("e",[])])
        //visualize tr |> printfn "%s"
        leafs tr = ["d"; "a"; "c"; "e"]

    let notleafs inputTree =
        let rec f acc = function
            | Node(x, []) -> acc
            | Node(x, xs) ->
                x :: List.foldBack (fun x state -> f state x) xs acc
        f [] inputTree
    assert
        let tr = Node ("b",[Node ("c",[Node ("d",[])]); Node ("d",[Node ("a",[]); Node ("c",[])]); Node ("e",[])])
        //visualize tr |> printfn "%s"
        notleafs tr = ["b"; "c"; "d"]
    let map func inputTree =
        let rec f = function
            | Node(x, []) -> Node(func x, [])
            | Node(x, xs) -> Node(func x, List.map f xs)
        f inputTree
    let cutLeaf inputTree =
        let rec f = function
            | Node(x, []) -> failwithf "%A leaf is root tree" <| Node(x, [])
            | Node(x, xs) ->
                Node(x, List.choose(function Node(_, []) -> None | x -> f x |> Some) xs)
        f inputTree
    assert
        let tr = Node ("b",[Node ("c",[Node ("d",[])]); Node ("d",[Node ("a",[]); Node ("c",[])]); Node ("e",[])])
        visualize (sprintf "%s") tr |> printfn "%s"
        let x = cutLeaf tr
        x |> visualize (sprintf "%s") |> printfn "%s"
        x = Node ("b",[Node ("c",[]); Node ("d",[])])

    /// распаковать в вид [[1;2]; [1;3]...]
    let rec unpack pairs =
        let f = function
            | Node(e, []) -> [[e]]
            | Node(e, L) -> List.map (fun x -> e::x) (unpack L)
        List.collect f pairs // List.map f pairs |> List.concat
    /// <summary>Запаковать [[1..3]; [2; 3]] в [Node (1,[Node (2,[Node (3,[])])]); Node (2,[Node (3,[])])] </summary>
    let rec pack = function
        | [] -> []
        | xs ->
            let f =
                List.filter (List.length >> (<>) 0)
                >> Seq.groupBy List.head
                >> Seq.map (mapSnd (Seq.map List.tail >> List.ofSeq))
            Seq.map (mapSnd pack >> Node) (f xs) |> List.ofSeq
    assert
        let xs = [[1..3]; [2..5]; [4..10]]
        pack xs |> unpack = xs

    module group =
        type 'a Val = Val of 'a | Ref of int
        let rec find = function
            | Node(_, []) -> None
            | Node(_, xs) & curr -> if List.forall (function Node(_, []) -> true | _ -> false) xs then Some curr else List.tryPick find xs
        let replaces i rep tree =
            let rec f = function
                | Node(name, xs) & curr -> if curr = rep then Node(Ref i, []) else Node(name, List.map f xs)
            f tree

        let group tree =
            let rec f i acc tree = find tree |> function Some finded -> replaces i finded tree |> (fun x -> f (i + 1) ((i,finded)::acc) x) | None -> acc
            f 0 [] (map Val tree)

    module group2 =
        type 'a Val = Val of 'a | Ref of int * 'a
        let getVal = function Val x -> x | x -> failwithf "expected `Val _`, but actual: `%A`" x
        /// Оно ищет Node в котором узлы пусты
        let rec find = function
            | Node(_, []) -> None
            | Node(_, xs) & curr ->
                let isEmpty = function Node(_, []) -> true | _ -> false
                if List.forall isEmpty xs then Some curr
                else List.tryPick find xs
        let replaces i rep tree =
            let rec f = function
                | Node(name, xs) & curr ->
                    if curr = rep then Node(Ref i, [])
                    else Node(name, List.map f xs)
            f tree

        type 'a Leaf = (int * 'a) * (int * 'a)
        type 'a Graph = 'a Leaf list
        let group tree : 'a Graph =
            let iter xs i =
                List.foldBack (fun x (st, i) ->
                    match getValue x with
                    | Val x -> (i, x)::st, i + 1
                    | Ref(x, y) -> (x,y)::st, i ) xs ([], i)
            let rec f i acc tree =
                find tree |> function
                    | Some finded ->
                        (i, getValue finded |> getVal)
                        |> fun curr ->
                        replaces curr finded tree |> fun x ->
                        finded |> getBranch |> fun xs ->
                        iter xs (i+1) |> fun (xs,i) -> f i ((curr, xs)::acc) x
                    | None -> acc
            // f 0 [] (map Val tree) |> List.map (fun (x,xs) -> xs |> List.map (fun y -> x, y)  ) |> List.concat
            f 0 [] (map Val tree)
            |> List.collect (fun (x,xs) -> xs |> List.map (fun y -> x, y))

        let toTgf (xs : 'a Graph) =
            let f xs =
                xs |> Seq.map (fun (x, y) -> sprintf "%d %A" x y)
                |> String.concat "\n"
            xs |> List.fold (fun (xs, st) (x, y) ->
                    (fst x, fst y) :: xs, st |> Set.add x |> Set.add y )
                ([], Set.empty)
            |> fun (nodes, def) -> sprintf "%s\n#\n%s" (f def) (f nodes)