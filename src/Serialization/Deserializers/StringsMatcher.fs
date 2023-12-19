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
module FsharpMyExtension.Serialization.Deserializers.StringsMatcher
open FsharpMyExtension
open FsharpMyExtension.Collections

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

let lookup(from, max, deep, xs: (string * 'Value) []) =
    let current = fst xs.[from]
    if deep < current.Length then
        let firstChar = current.[deep]
        let rec loop from =
            if from < max then
                let currentChar = (fst xs.[from]).[deep]
                if currentChar = firstChar then
                    loop (from + 1)
                else
                    from
            else
                from

        Some (firstChar, loop (from + 1))
    else
        None

/// Значительно быстрее обобщенной реализации, да и память меньше ест
let toDicStrings(xs: (string * 'Value) []): Container<char, 'Value> =
    let rec f(from, max, deep, acc): Container<char, 'Value> =
        if from < max then
            let firstWord, firstValue = xs.[from]

            let from, acc =
                match lookup(from, max, deep, xs) with
                | Some(firstChar, max) ->
                    let v =
                        let from, v =
                            if deep + 1 < firstWord.Length then
                                from, None
                            else
                                from + 1, Some firstValue

                        let res = f(from, max, deep + 1, Map.empty)

                        Dic(v, res)

                    max, Map.add firstChar v acc

                | None ->
                    failwithf "the list contains the same words: %A" xs.[from]

            f(from, max, deep, acc)
        else
            acc

    f(0, xs.Length, 0, Map.empty)

module Serializator =
    let leftChar = '←'
    let rightChar = '→'
    let upChar = '↑'
    let downChar = '↓'

    open FsharpMyExtension.Serialization.Serializers.ShowList

    let serialize (dic:Container<_,_>) =
        let rec f (dic:Container<_,_>) =
            dic
            |> Map.fold (fun st k v ->
                let rests =
                    v
                    |> fun (Dic(k, v)) ->
                        match k with
                        | Some _ ->
                            showChar upChar
                        | None -> empty
                        << if Map.isEmpty v then empty
                           else
                               showChar rightChar
                               << f v
                               << showChar leftChar
                st << showChar k << rests
            ) empty
        f dic
        |> show

    open FParsec

    type State = {
        PrevIndent : System.Text.StringBuilder
    }
    let many1Map p =
        Inline.Many(
           stateFromFirstElement = (fun x -> Map [x]), // : 'T -> 'State  *
           foldState             = (fun st (k, v) -> Map.add k v st),  // 'State -> 'T -> 'State  *
           resultFromState       = id,  // 'State -> 'Result  *
           elementParser         = p  // Parser<'T,'U>  *
        )

    let pdeserialize: Parser<Container<_,_>, _> =
        let pleft =
            pchar leftChar

        let pright  =
            pchar rightChar

        let p, pref = createParserForwardedToRef()
        let p2 =
            satisfy (fun c -> not (c = leftChar || c = rightChar))
            >>= fun c ->
                updateUserState (fun x ->
                    { PrevIndent = x.PrevIndent.Append c })
                >>. preturn c
            .>>. opt (pchar upChar
                      >>. getUserState
                      |>> fun x -> x.PrevIndent.ToString())
            .>>. opt (between pright pleft p)
            .>> updateUserState (fun x ->
                let sb = x.PrevIndent
                { PrevIndent = sb.Remove(sb.Length - 1, 1) })
            |>> fun ((c, word), x) ->
                match x with
                | Some xs ->
                    (c, Dic(word, xs))
                | None ->
                    (c, Dic(word, Map.empty))
        pref := many1Map p2
        p
    let deserialize str =
        let emptyState = { PrevIndent = System.Text.StringBuilder() }
        match runParserOnString (pdeserialize .>> eof) emptyState "" str with
        | Success(x, _, _) -> x
        | Failure(x, _, _) -> failwithf "%A" x
    let deserializeFile path =
        let emptyState = { PrevIndent = System.Text.StringBuilder() }
        match runParserOnFile (pdeserialize .>> eof) emptyState path System.Text.Encoding.UTF8 with
        | Success(x, _, _) -> x
        | Failure(x, _, _) -> failwithf "%A" x

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

let toTree keyf (xs: Container<_, _>) =
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

    /// Пытается сопоставить первый символ хоть с чем-то из словаря.
    let pcharFromDic mapping label (m: Container<_, _>) : Parser<_, _> =
        let f =
            fun (stream : CharStream<'UserState>) ->
                let curr = stream.Peek() |> mapping
                match Map.tryFind curr m with
                | Some(x) ->
                    stream.Read() |> ignore
                    Reply((curr, x))
                | None ->
                    // ```fsharp
                    // let label =
                    //     m
                    //     |> Seq.map (fun (KeyValue(k, _)) -> sprintf "%A" k)
                    //     |> String.concat " or "
                    // ```
                    // — это хорошая мысль, но `mapping` всё портит.
                    // Например, если словарь — `["a"]`, mapping задан как `fun _ -> 'b'` и на вход подаем `"a"`, то функция вернет:
                    // ```
                    //  Error in Ln: 1 Col: 1
                    //  a
                    //  ^
                    //  Expecting: 'a'
                    // ```
                    // Правда, обескураживает? То-то же.
                    let errorMsg =
                        FParsec.Error.expected label
                    Reply(ReplyStatus.Error, errorMsg)
        f
