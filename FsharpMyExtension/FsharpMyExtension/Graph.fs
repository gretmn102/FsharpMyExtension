module FsharpMyExtension.Graph
open FsharpMyExtension

type 'a Leaf = 'a * 'a
// type 'a Graph when 'a : comparison = Set<'a Leaf>
type 'a Graph = 'a Leaf list

/// https://en.wikipedia.org/wiki/Connected_component_(graph_theory)
/// Имеется неориентированный граф, несвязный и возможно цикличный. Нужно перечислить все "компоненты связности".
/// Зачем? Положим следующий список уравнений:
/// `[a = b; a = c; e = d; d = f;]`
/// Вдруг захотелось узнать, что `[[a = b = c]; [d = e = f]]` — этот алгоритм самое то для решения такой задачи.
/// Стоит ли говорить, что алгоритм крайне тяжелый?
let connectedComponents (gr:_ Graph) = 
    let f xs =
        xs
        |> List.fold (fun st (x, y) ->
            st
            |> Map.addOrModWith x (fun _ -> Set.singleton y) (Set.add y)
            |> Map.addOrModWith y (fun _ -> Set.singleton x) (Set.add x)
        ) Map.empty
    f gr
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