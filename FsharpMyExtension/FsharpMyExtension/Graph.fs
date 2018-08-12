module FsharpMyExtension.Graph
open FsharpMyExtension

type 'a Leaf = 'a * 'a
// type 'a Graph when 'a : comparison = Set<'a Leaf>
type 'a Graph = 'a Leaf list

/// Списки инцидентности (Adjacency list)
/// https://ru.wikipedia.org/wiki/Список_смежности
let toAdjacencys xs =
    xs
    |> List.fold (fun st (x, y) ->
        st
        |> Map.addOrModWith x (fun _ -> Set.singleton y) (Set.add y)
        |> Map.addOrModWith y (fun _ -> Set.singleton x) (Set.add x)
    ) Map.empty
/// https://en.wikipedia.org/wiki/Connected_component_(graph_theory)
/// Имеется неориентированный граф, несвязный и возможно цикличный. Нужно перечислить все "компоненты связности".
/// Зачем? Положим следующий список уравнений:
/// `[a = b; a = c; e = d; d = f;]`
/// Вдруг захотелось узнать, что `[[a = b = c]; [d = e = f]]` — этот алгоритм самое то для решения такой задачи.
/// Стоит ли говорить, что алгоритм крайне тяжелый?
let connectedComponents (gr:_ Graph) = 
    toAdjacencys gr
    |> List.unfold (fun m ->
        match Seq.tryHead m with
        | Some(KeyValue(k,_)) ->
            let rec loop (acc, m) x =
                match Map.tryFind x m with
                | Some ys ->
                    ys |> Seq.fold loop (Set.add x acc, Map.remove x m)
                | None -> Set.add x acc, m
            let x, m = loop (Set.empty, m) k
            Some(x, m)
        | None -> None
    )
assert
    let xs : Set<int> list = connectedComponents []
    [] = xs
assert
    let xs = connectedComponents ['a', 'b'; 'b', 'c']
    [set ['a'; 'b'; 'c'];] = xs
assert
    let xs = connectedComponents ['a', 'b'; 'a', 'c'; 'e', 'd'; 'd', 'f';]
    [set ['a'; 'b'; 'c']; set ['d'; 'e'; 'f']] = xs
assert
    let xs = connectedComponents ['a', 'c'; 'd', 'e';'f','a'; 'd', 'f'; 'g', 'h'; 'h', 'x']
    [set ['a'; 'c'; 'd'; 'e'; 'f']; set ['g'; 'h'; 'x']] = xs

module group =
    open FsharpMyExtension.Tree
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
        f 0 [] (Tree.map Val tree)

module group2 =
    open FsharpMyExtension.Tree
    /// Найти первый попавшийся узел, который содержит только листья.
    /// `1, [2, [3]; 4, [5; 6]]` -> `2, [3]`
    let rec find = function
        | Node(_, []) -> None
        | Node(_, xs) & curr ->
            let isEmpty = function Node(_, []) -> true | _ -> false
            if List.forall isEmpty xs then Some curr
            else List.tryPick find xs
    assert
        let (<!>) x xs = Node(x, xs)
        let nod = 
            1 <!> [
                    2 <!> [Tree.singleton 3]
                    4 <!> [
                          Tree.singleton 5
                          Tree.singleton 6
                          ]
                  ]
        // nod |> Tree.visualize (sprintf "%d") |> Clipboard.setText
        Some(Node(2, [Tree.singleton 3])) = find nod
        
    type 'a Val = Val of 'a | Ref of int * 'a
    let getVal = function Val x -> x | x -> failwithf "expected `Val _`, but actual: `%A`" x
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
                match Tree.getValue x with
                | Val x -> (i, x)::st, i + 1
                | Ref(x, y) -> (x, y)::st, i ) xs ([], i)
        let rec f i acc tree =
            find tree |> function
                | Some found ->
                    (i, Tree.getValue found |> getVal)
                    |> fun curr ->
                    replaces curr found tree |> fun x ->
                    found |> Tree.getBranch |> fun xs ->
                    iter xs (i+1) |> fun (xs,i) -> f i ((curr, xs)::acc) x
                | None -> acc
        // f 0 [] (map Val tree) |> List.map (fun (x,xs) -> xs |> List.map (fun y -> x, y)  ) |> List.concat
        f 0 [] (Tree.map Val tree)
        |> List.collect (fun (x,xs) -> xs |> List.map (fun y -> x, y))

    let toTgf (xs : 'a Graph) =
        let f xs =
            xs |> Seq.map (fun (x, y) -> sprintf "%d %A" x y)
            |> String.concat "\n"
        xs |> List.fold (fun (xs, st) (x, y) ->
                (fst x, fst y) :: xs, st |> Set.add x |> Set.add y )
            ([], Set.empty)
        |> fun (nodes, def) -> sprintf "%s\n#\n%s" (f def) (f nodes)