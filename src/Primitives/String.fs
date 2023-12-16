// [<RequireQualifiedAccess>]
module FsharpMyExtension.Primitives.String
let split (sep:string) (s:string) = s.Split([|sep|], System.StringSplitOptions.None)
let splitSeq (sep:string) (x:string) =
    let rec f (i:int) =
        if i < x.Length then
            match x.IndexOf(sep, i) with
            | -1 -> seq{ yield x.Substring i }
            | j ->
                seq{
                    yield x.Substring(i, j - i)
                    yield! f (j + sep.Length)
                }
        else Seq.empty
    f 0
/// split [|"\r\n"; "\n"; "\r"|] s
let lines (s:string) = s.Split([|"\r\n"; "\n"; "\r"|], System.StringSplitOptions.None)
let replace (oldVal:string) newVal (s:string) = s.Replace(oldVal, newVal)
/// remove all white-spaces leading and trailing
let trim (s:string) = s.Trim()
let trimChar trimChar (s:string) = s.Trim [| trimChar |]
let trimChars trimChars (s:string) = s.Trim trimChars
let toLower (s:string) = s.ToLower()
let contains value (s:string) = s.Contains value
/// `TRACE` | `tRaCe` -> `Trace`
let firstCapital (x:string) =
    if x = "" then ""
    else
        // TODO: optimize
        string (System.Char.ToUpper x.[0]) + String.map System.Char.ToLower x.[1..]

/// **Exceptions**
/// * System.ArgumentException: The input string was empty.
let last = function
    | "" -> raise (System.ArgumentException("The input string was empty."))
    | str -> str.[str.Length - 1]

/// Строит таблицу. Пример:
/// `table 4 [["a"; "bc"; "e"]; ["f"; "ghx"]; [""; ""; "df"]; ["sdkjf"; "as"; "fkj"; "jaf"]]`
/// Вернет:
/// ```
/// a        bc     e
/// f        ghx
///                 df
/// sdkjf    as     fkj    jaf
/// ```
let createTable width lines =
    let mapping fBoth fxs =
        let join xs ys = List.fold (fun xs x -> x::xs) ys xs
        let rec red acc = function
            | x::xs, y::ys ->
                red (fBoth x y :: acc) (xs,ys)
            | xs, [] ->
                join acc (List.map fxs xs)
            | [], xs ->
                join acc xs
        red []
    let ls =
        lines
        |> List.fold (fun st xs ->
            mapping (fun x y -> max (String.length x) y ) String.length (xs,st) ) []
    let ls = List.rev ls |> function _::xs -> 0::xs |> List.rev | [] -> []

    let swidth = String.replicate width " "
    lines
    |> List.map (Seq.map2 (fun l x -> x.PadRight l) ls >> String.concat swidth)
    |> String.concat "\n"
// open FsharpMyExtension
let createTableTest() =
    let xs =
        [
            "([волк;коза;капуста],[])"
            "->\tкоза\t([волк;капуста],[])"
            "<-\t\t([волк;капуста], [коза])"
            "->\tкапуста\t([волк], [коза])"
            "<-\tкоза\t([волк], [капуста])"
            "->\tволк\t([коза], [капуста])"
            "<-\t\t([коза], [волк;капуста])"
            "->\tкоза\t([], [волк;капуста])"
            "([],[волк;коза;капуста])"
        ]
        |> List.map (fun s ->
            s.Split([| "\t"|], System.StringSplitOptions.None)
            |> List.ofArray)
    let exp =
        [
            "([волк;коза;капуста],[])"
            "->                          коза       ([волк;капуста],[])"
            "<-                                     ([волк;капуста], [коза])"
            "->                          капуста    ([волк], [коза])"
            "<-                          коза       ([волк], [капуста])"
            "->                          волк       ([коза], [капуста])"
            "<-                                     ([коза], [волк;капуста])"
            "->                          коза       ([], [волк;капуста])"
            "([],[волк;коза;капуста])"
        ] |> String.concat "\n"
    let act = xs |> createTable 4
    exp = act

let chunkBySize (chunkSize: int) (str: string) =
    let elementsCount = str.Length
    let chunksCount = elementsCount / chunkSize

    if elementsCount % chunkSize = 0 then
        let arr = Array.zeroCreate chunksCount
        for i = 0 to chunksCount - 1 do
            arr.[i] <- str.Substring(chunkSize * i, chunkSize)
        arr
    else
        let chunksCount = chunksCount + 1

        let arr = Array.zeroCreate chunksCount
        for i = 0 to (chunksCount - 1) - 1 do
            arr.[i] <- str.Substring(chunkSize * i, chunkSize)

        let lastIndex = chunksCount - 1
        let restCount = elementsCount - (chunkSize * lastIndex)
        arr.[lastIndex] <- str.Substring(chunkSize * lastIndex, restCount)

        arr
