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
type Container<'Key, 'Value> when 'Key : comparison =
    Map<'Key, Dic<'Key, 'Value>>
and Dic<'Key, 'Value> when 'Key : comparison =
    | Dic of 'Value option * Container<'Key, 'Value>

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
            |> List.fold (fun m (k, v) ->
                Map.add k (slow <| List.map (mapFst List.tail) v) m) Map.empty
            |> fun m -> Dic(emptys, m)
    xs
    |> List.map (fun (x, y) -> List.ofSeq x, [y])
    |> slow
    |> fun (Dic(_, m)) -> m

/// Значительно быстрее обобщенной реализации, да и память меньше ест
let toDicStrings xs =
    let dicEmpty = Dic(None, Map.empty)
    let rec slow i (xs:list<string * _ list>) =
        if Seq.isEmpty xs then dicEmpty
        else
            let emptys, others =
                xs
                |> List.partition (
                    fst >> (fun (x:string) -> not (i < x.Length) ) )
            let emptys =
                List.collect snd emptys
                |> function
                    | [x] -> Some x
                    | [] -> None
                    | xs -> failwithf "в списке есть одинаковые ключи:\n%A" xs
            others
            |> List.groupBy (fst >> (fun x -> x.[i]))
            |> List.fold (fun m (k, v) ->
                Map.add k (slow (i + 1) v) m) Map.empty
            |> fun m -> Dic(emptys, m)
    xs
    |> List.map (fun (x, y) -> x, [y])
    |> slow 0
    |> fun (Dic(_, m)) -> m

/// Не жадный способ.
///
/// Например, если правила `["ab"; "abc"]`,
/// а входная последовательность — `abc`,
/// то функция вернет первое правило.
let rec runOnListNotGreedy m = function
    | curr::xs ->
        match Map.tryFind curr m with
        | Some (Dic(v, m)) ->
            match v with
            | None ->
                xs |> runOnListNotGreedy m
            | Some value -> Some value
        | None -> None
    | [] -> None

/// Жадный способ.
///
/// Например, если заданы правила как `["ab"; "abc"]`,
/// а входная последовательность — `abc`,
/// то функция вернет второе правило.
let runOnListGreedy m =
    let rec f last m = function
        | curr::xs ->
            match Map.tryFind curr m with
            | Some (Dic(v, m)) ->
                match v with
                | None ->
                    match last with
                    | None ->
                        f None m xs
                    | Some last -> f (Some last) m xs
                | Some value ->
                    f (Some value) m xs
            | None -> last
        | [] -> last
    f None m

open FsharpMyExtension
// OPTIMIZE
let rec runOnSeqNotGreedy m (xs:_ seq) =
    if Seq.isEmpty xs then None
    else
        let curr, xs = Seq.headTail xs
        match Map.tryFind curr m with
        | Some (Dic(v, m)) ->
            match v with
            | None ->
                xs |> runOnSeqNotGreedy m
            | Some value -> Some value
        | None -> None

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

module FParsec =
    open FParsec
    let keywordsL m label : Primitives.Parser<'Result, 'UserState> =
        fun (stream : CharStream<'UserState>) ->
            let st = stream.State
            let rec f last m =
                if stream.IsEndOfStream then
                    last
                else
                    let curr = stream.Peek()
                    match Map.tryFind curr m with
                    | Some (Dic(v, m)) ->
                        stream.Read() |> ignore
                        match v with
                        | None ->
                            f last m
                        | Some value ->
                            f (Some (stream.State, value)) m
                    | None -> last
            match f None m with
            | Some(st, x) ->
                stream.BacktrackTo st
                Reply(x)
            | None ->
                stream.BacktrackTo st
                let errorMsg =
                    FParsec.Error.expected label
                Reply(ReplyStatus.Error, errorMsg)
