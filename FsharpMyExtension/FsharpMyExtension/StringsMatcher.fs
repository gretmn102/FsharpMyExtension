/// Часто бывает, что нужно сопоставить строчный тип с кучей образцов. Например, есть такое:
/// ```fsharp
/// match str with
/// | "second" -> ...
/// | "first" -> ...
/// | "firmest" -> ...
/// | "sec" -> ...
/// | _ -> ...
/// ```
/// И таких строк может быть куча. Конечно, можно их запихнуть в Map и оттуда доставать необходимое, что значительно сократит время, но можно еще сократить через общие вхождения символов. В нашем примере это будет "**fir**" и "**fir**mest"; "**sec**" и "**sec**ond", то есть теперь достаточно сопоставить два варианта:
///
module FsharpMyExtension.StringsMatcher
// type Dic<'Key, 'Value> when 'Key : comparison =
//     | DicValue of 'Value * Map<'Key, Dic2<'Key, 'Value>>
//     | Dic of Map<'Key, Dic<'Key, 'Value>>
//     | Leaf of 'Value
type Dic<'Key, 'Value> when 'Key : comparison =
    | Dic of 'Value option * Map<'Key, Dic<'Key, 'Value>>

// OPTIMIZE
let toDic xs =
    let dicEmpty = Dic(None, Map.empty)
    let rec slow xs =
        if Seq.isEmpty xs then dicEmpty
        else
            let emptys, others =
                xs |> List.partition (fst >> Seq.isEmpty)
            let emptys =
                List.collect snd emptys
                |> function
                    | [x] -> Some x
                    | [] -> None
                    | xs -> failwithf "в списке есть одинаковые ключи:\n%A" xs
            others
            |> List.groupBy (fst >> List.head)
            |> List.map (fun (k, v) ->
                (k, slow <| List.map (mapFst List.tail) v))
            |> fun m -> Dic(emptys, Map.ofList m)
    xs
    |> List.map (fun (x, y) -> List.ofSeq x, [y])
    |> slow
    |> fun (Dic(_, m)) -> m
assert
    let input =
        [
            "a", "first"
            "ab", "second"
            "ac", "third"
        ]

    let output =
        Map
          [('a',
            Dic
              (Some "first",
               Map
                 [('b', Dic (Some "second", Map []));
                  ('c', Dic (Some "third", Map []))]))]
    let input2 =
        [
            "second", "second"
            "first", "first"
            "firmest", "firmest"
            "sec", "sec"
        ]
    // toDic input2

    toDic input = output

/// Не жадный способ.
///
/// Например, если правила `["ab"; "abc"]`,
/// а входная последовательность — `abc`,
/// то функция вернет первое правило.
let rec runOnList m = function
    | curr::xs ->
        m
        |> Seq.tryPick (fun (KeyValue(k, (Dic(v, m)))) ->
            match v with
            | None ->
                if k = curr then runOnList m xs
                else None
            | Some x -> Some x)
    | [] -> None

open FsharpMyExtension
// OPTIMIZE
let rec runOnSeq m (xs:_ seq) =
    if Seq.isEmpty xs then None
    else
        let curr, xs = Seq.headTail xs
        m
        |> Seq.tryPick (fun (KeyValue(k, (Dic(v, m)))) ->
            match v with
            | None ->
                if k = curr then runOnSeq m xs
                else None
            | Some x -> Some x)

open FsharpMyExtension.Tree
let toTree keyf xs =
    let rec f (Dic(v, m)) =
        match v with
        | None ->
            m
            |> Seq.map (fun (KeyValue(k, v)) ->
                Node(keyf k, f v))
            |> List.ofSeq
        | Some v -> [ Node(v, []) ]
    xs
    |> Seq.map (fun (KeyValue(k, v)) ->
        Node(keyf k, f v))
    |> List.ofSeq
