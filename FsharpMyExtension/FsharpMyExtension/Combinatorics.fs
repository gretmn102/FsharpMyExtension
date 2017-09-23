namespace FsharpMyExtension.Combinatorics

type 'a LazyTree = Node of 'a * 'a LazyTree seq
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module LazyTree = 
    let visualize print inputTree = 
        let prefMid = seq { yield "├─"; while true do yield "│ " }
        let prefEnd = seq { yield "└─"; while true do yield "  " }
        let prefNone = seq { while true do yield "" }
 
        let inline c2 x y = Seq.map2 (+) x y

        let rec visualize (Node(label:'a, children:LazyTree<'a> seq)) pre =
            seq {
                yield Seq.head pre + print label
                if not <| Seq.isEmpty children then
                    let preRest = Seq.skip 1 pre
                    let last = Seq.last children
                    for e in children do
                        if e = last then yield! visualize e (c2 preRest prefEnd)
                        else yield! visualize e (c2 preRest prefMid)
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
    /// <summary> распаковать в вид [[1;2]; [1;3]...] </summary>
    let rec unpack pairs =
        let f = function
            | Node(e, L) when Seq.isEmpty L -> [[e]]
            | Node(e, L) -> List.map (fun x -> e::x) (unpack L)
        Seq.map f pairs |> List.concat
    assert
        let dummy = 
             [Node (0,seq [Node (1,seq []); Node (2,seq []); Node (3,seq [])]);
             Node (1,seq [Node (2,seq []); Node (3,seq [])]);
             Node (2,seq [Node (3,seq [])])]
        unpack dummy = [[0; 1]; [0; 2]; [0; 3]; [1; 2]; [1; 3]; [2; 3]]
module Comb =
    let rec perm count = 
        if count <= 0 then fun _ -> Seq.empty
        else
            let fn = function
                | [] -> None
                | h::t as xs ->
                    if List.length xs < count then None
                    else Some(Node(h, perm <| count - 1 <| t), t)
            Seq.unfold fn
    assert
        perm 0 [0..4] |> Seq.isEmpty
    assert
        let xs = [1..4]
        perm 1 xs |> List.ofSeq = List.map (fun x -> Node(x, Seq.empty)) xs
    assert
        perm 2 [0..3] |> LazyTree.unpack = [[0; 1]; [0; 2]; [0; 3]; [1; 2]; [1; 3]; [2; 3]]
    assert
        perm 4 [0..3] |> LazyTree.unpack = [[0..3]]
    assert
        perm 5 [0..3] |> Seq.isEmpty
//    let rec permm xs =
//        let fn = function
//            | [] -> None
//            | h::t as xs -> Some(Node(h, seq{ yield Nil; yield! permm t}), t)
//        Seq.unfold fn xs
//    assert
//        
//        permm [0..3] |> LazyTree.unpack
//    let xs = Seq.init 10 (printfn "%d")
//    Seq.take 2 xs
//    Seq.skip 2 xs




//module SandBox = 
//    open FsControl
//    let sdf xs = Operators.map id xs
//    
//    (Some 1) >>= (+) 1