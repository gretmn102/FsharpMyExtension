[<RequireQualifiedAccess>]
module FsharpMyExtension.Array

let transpose xss =
    let h = Array.length xss
    let w = Array.head xss |> Array.length
    let yss = Array.init w (fun _ -> Array.zeroCreate h)

    // for i = 0 to Array2D.length1 xss - 1 do
    //     for j = 0 to Array2D.length2 xss - 1 do
    //         yss.[i].[j] <- xss.[i,j]
    xss |> Array.iteri (fun i -> Array.iteri(fun j x -> yss.[j].[i] <- x))

    // Array2D.iteriP (fun i j x -> yss.[i].[j] <- x) xss
    yss
/// Находит подмассив в массиве.
let indexOf patt startIndex xs =
    let rest xs ys =
        if Array.length xs < Array.length ys then false
        else
            Seq.zip xs ys
            |> Seq.forall (fun (x, y) -> x = y)
    // true = rest [| 0..5 |] [| 0..4 |]
    // false = rest [| 0;1;2 |] [| 0; 2 |]
    let isMatch (xs: _ []) ys startIndex =
        let rec loop i =
            if i < Array.length ys then
                if xs.[i + startIndex] <> ys.[i] then false
                else loop (i + 1)
            else true
        loop 0
    // isMatch [| 0..5 |] [| 0..4 |] 0
    // let len = Array.length xs

    // let patt, startIndex, xs = [| 0..10 |], 0, [| 1; 2 |]
    let count = Array.length xs - Array.length patt + 1
    // printfn "count = %d" count
    let headPatt, restPatt = Array.head patt, Array.tail patt
    let rec f i =
        // printfn "i = %d" i
        let i = System.Array.IndexOf(xs, headPatt, i, count - i)
        // printfn "i2 = %d" i
        if i = -1 then -1
        elif isMatch xs restPatt (i + 1) then i
        else f (i + 1)
    f startIndex
// 1 = indexOf [| 1; 2 |] 0 [| 0..10 |]
// 1 = indexOf [|2;3|] 0 [|1..6|]

/// Ведет себя почти так же, как и `System.String.Split`, за исключением:
/// ```fsharp
/// let sep = "<>"
/// let str = "<>"
/// str.Split([|str|], System.StringSplitOptions.None) // -> [|""; ""|]
/// split (sep.ToCharArray()) (str.ToCharArray())
/// |> Array.map System.String.Concat // -> [|""|]
/// ```
/// Еще пример:
/// ```fsharp
/// let sep = "<>"
/// let str = "asdfg<> s aff<>a<>fa<>f<><>dsf"
/// split (sep.ToCharArray()) (str.ToCharArray())
/// |> Array.map System.String.Concat
/// // -> [|"asdfg"; " s aff"; "a"; "fa"; "f"; ""; "dsf"|]
/// ```
let split (sep:_ []) (xs:_ []) =
    Array.unfold (fun i ->
        if i < xs.Length then
            let i' = indexOf sep i xs
            if i' = -1 then Some(xs.[i..], xs.Length)
            else
                Some(xs.[i..i' - 1], i' + sep.Length)
        else
            None
        ) 0

let mapStartMidEnd start mid fend (xs: _ []) =
    let length = xs.Length

    let ys = Array.zeroCreate length
    if length > 0 then
        ys.[0] <- start xs.[0]
        if length > 1 then
            ys.[length - 1] <- fend xs.[length - 1]
            for i in 1..length - 2 do
                ys.[i] <- mid xs.[i]
    ys

/// Create new array without element by target index.
let removeAt i (xs: _ []) =
    Array.init (xs.Length - 1) (fun i' ->
        if i' < i then xs.[i']
        else  xs.[i' + 1]
    )

/// Generates the array of `length` random integers from `m1` to `m2` (inclusive), giving a sum of `sum`.
///
/// Examples:
/// ```fsharp
/// generateRandomNumbersBySum (1, 4) 10 3 // -> [| 3; 3; 4 |]
/// generateRandomNumbersBySum (1, 4) 10 5 // -> [| 2; 2; 1; 4; 1 |]
/// generateRandomNumbersBySum (1, 6) 16 5 // -> [| 4; 3; 2; 1; 6 |]
/// generateRandomNumbersBySum (3, 6) 20 6 // -> [| 3; 4; 3; 3; 3; 4 |]
/// ```
let generateRandomNumbersBySum length (m1, m2) sum =
    if m1 > m2 then failwith "m1 > m2"
    if sum < m1 * length then failwith "sum < m1 * length"
    if sum > m2 * length then failwith "sum > m2 * length"
    let r = System.Random()
    let xs = Array.replicate length m1
    for __ = (length * m1) to sum - 1 do
        let rec f () =
            let i = r.Next(0, length)
            let x = xs.[i]
            if x < m2 then
                xs.[i] <- x + 1
            else
                f()
        f()
    xs

let swap sourceId targetId xs =
    let ys = Array.copy xs

    if sourceId < targetId then
        let startId = sourceId + 1
        let length = targetId - sourceId
        for i = startId to startId + length - 1 do
            ys.[i - 1] <- xs.[i]
    else
        let startId = targetId
        let length = sourceId - targetId
        for i = startId to startId + length - 1 do
            ys.[i + 1] <- xs.[i]
    ys.[targetId] <- xs.[sourceId]
    ys
