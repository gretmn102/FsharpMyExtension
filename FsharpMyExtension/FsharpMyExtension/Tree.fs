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
    let rec ofLazyTree (LT(x,xs)) =
        Node(x, xs |> Seq.map ofLazyTree |> List.ofSeq)