namespace FsharpMyExtension

[<Struct>]
type 'a LazyTree = LT of 'a * 'a LazyTree seq

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module LazyTree =
    let visualize print inputTree = 
        let prefMid = seq { yield "├─"; while true do yield "│ " }
        let prefEnd = seq { yield "└─"; while true do yield "  " }
        let prefNone = seq { while true do yield "" }
 
        let inline c2 x y = Seq.map2 (+) x y

        let rec visualize (LT(label:'a, children:LazyTree<'a> seq)) pre =
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
            LT ("root",
                    [LT ("a", 
                            [LT ("a1",
                                    [LT ("a11", []);
                                    LT ("a12", []) ]) ]);
                    LT ("b",
                            [LT ("b1", []) ]) ])
        visualize (sprintf "%s") dummy = "root\n├─a\n│ └─a1\n│   ├─a11\n│   └─a12\n└─b\n  └─b1"
    /// <summary> распаковать в вид [[1;2]; [1;3]...] </summary>
    let rec unpack pairs =
        let f = function
            | LT(x, xs) when Seq.isEmpty xs -> Seq.singleton [x]
            | LT(x, xs) -> Seq.map (fun xs -> x::xs) (unpack xs)
        Seq.collect f pairs
    // assert
    //     let dummy = 
    //          [Node (0,seq [Node (1,seq []); Node (2,seq []); Node (3,seq [])]);
    //           Node (1,seq [Node (2,seq []); Node (3,seq [])]);
    //           Node (2,seq [Node (3,seq [])])]
    //     unpack dummy = [[0; 1]; [0; 2]; [0; 3]; [1; 2]; [1; 3]; [2; 3]]
    /// Список списка преобразовывает в дерево при помощи `Seq.group`.
    /// Пример, `[[1;3]; [2;3]; [1;2]]` -> `[[1;2]; [1;3]; [2;3]]`
    /// Наверное стоило сделать, чтобы запаковывалось последовательно.
    /// В таком случае, выполнялось бы условие `pack >> unpack = pack`.
    /// TODO: Привести примеры для пущей ясности.
    let rec pack xs =
        if Seq.isEmpty xs then Seq.empty
        else
            xs
            |> Seq.filter (List.isEmpty >> not)
            |> Seq.groupBy List.head
            |> Seq.map (fun (k, v) -> LT(k, pack <| Seq.map (List.tail) v))
    

