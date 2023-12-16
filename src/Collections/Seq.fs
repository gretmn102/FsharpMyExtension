[<RequireQualifiedAccess>]
module FsharpMyExtension.Seq
open FsharpMyExtension.Collections

let rec transpose xss =
    if Seq.isEmpty xss || Seq.isEmpty (Seq.head xss) then
        Seq.empty
    else
        seq{
            yield Seq.map Seq.head xss
            yield! transpose (Seq.map Seq.tail xss)
        }

let headTail (xs: _ seq) =
    Seq.head xs, Seq.tail xs

let mapMidLast fmid flast (xs:_ seq) =
    seq{
        use xs = xs.GetEnumerator()
        if xs.MoveNext() then
            let mutable x = xs.Current
            while xs.MoveNext() do
                yield fmid x
                x <- xs.Current
            yield flast x
    }

let rec foldr f st (xs: _ seq) =
    use xs = xs.GetEnumerator()
    let rec foldr f st =
        if xs.MoveNext() then
            f xs.Current (lazy (foldr f st))
        else st
    foldr f st

/// Порой свёртке незачем проходить весь массив, и эта функция как раз для этого.
/// Она работает как обычный `.fold`, только возвращает последнее состояние, если указанная функция вернула `None`.
let rec scrappyFold fn (st:'State) (xs:_ seq) =
    use xs = xs.GetEnumerator()
    let rec f st =
        if xs.MoveNext() then
            let x = xs.Current
            match fn st x with
            | Some st -> f st
            | None -> st
        else st
    f st
let scrappyFoldTest () =
    let xs = [0..10]
    let exp =
        xs
        |> Seq.fold (fun st x ->
            if x < 6 then st + x
            else st
        ) 0
    let act =
        xs
        |> scrappyFold (fun st x ->
            if x < 6 then Some (st + x)
            else None ) 0
    exp = act

/// Делает то же самое, что и `scrappyFold`, только можно вернуть именно это состояние, если аргумент у функции — `true`.
let exactlyFold fn (st:'State) (xs:_ seq) =
    use xs = xs.GetEnumerator()
    let rec f st =
        if xs.MoveNext() then
            let x = xs.Current
            match fn st x with
            | false, st -> f st
            | true, st -> st
        else st
    f st

let travOpt fn (xs : _ seq) =
    let rec f acc (xs:System.Collections.Generic.IEnumerator<_>) =
        if xs.MoveNext() then
            match xs.Current |> fn with
            | None -> None
            | Some x -> f (x::acc) xs
        else List.rev acc |> Some
    f [] <| xs.GetEnumerator()

let seqOpt xs = travOpt id xs

let concatSep sep (xs:_ seq) =
    seq {
        use xs = xs.GetEnumerator()
        if xs.MoveNext() then
            let mutable x = xs.Current
            yield x
            while xs.MoveNext() do
                yield sep
                x <- xs.Current
                yield x
    }

let r = System.Random()

/// Generate sequence with random number from 0 to length. For example:
/// ```fsharp
/// generateRandomSequence 4
/// ```
/// return `seq [1; 3; 0; 2]` or `seq [1; 2; 0; 3]`, etc.
let generateRandomSequence length =
    let rec f length (xs: int []) =
        seq {
            if length > 0 then
                let n = r.Next(0, length)

                yield xs.[n]
                yield! f (length - 1) (Array.removeAt n xs)
        }

    f length [|0..length - 1|]
