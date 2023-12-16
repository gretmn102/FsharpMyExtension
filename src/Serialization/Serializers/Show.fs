namespace FsharpMyExtension

module Show =
    type String = char seq
    type ShowS = String -> String

    let split sep (str: string) = str.Split([|sep|])

    let showChar c = Seq.append (Seq.singleton c) : ShowS

    let showString xs = Seq.append xs : ShowS

    let empty = id : ShowS

    let (nl: ShowS) = showString System.Environment.NewLine

    let between (opened: ShowS) (closed: ShowS) (p: ShowS) = (opened << p << closed) : ShowS

    let showParen b = (cond (k b) (between (showChar '(') (showChar ')')) id) : (ShowS -> ShowS)

    let bet opened closed (p: ShowS) = (showString opened << p << showString closed) : ShowS

    let show (f: ShowS) = System.String.Concat(f Seq.empty)

    let shows x = (showString (x.ToString())): ShowS

    let showAutoParen parOpen: (_ -> ShowS) =
        let parClose = function
            | "(" -> ")"
            | "[" -> "]"
            | "{" -> "}"
            | x -> x

        bet parOpen (parClose parOpen)

    assert
        let s = showAutoParen "("
        show (s (showChar 'a') << s (showChar 'b')) = "(a)(b)"

    let join s =
        let join s =
            cond List.isEmpty (k empty) (List.reduce (fun x y -> x << (s << y)))
        join (showString s) : ShowS list -> ShowS

    assert
        [
            join " + " [] |> show = ""
            join " + " [ showString "1" ] |> show = "1"
            join " + " [ showString "1"; showString "2" ] |> show = "1 + 2"
            show (join " + " [ showString "someVar"; showChar '1'; showAutoParen "(" <| showString "2 + 3" ]) = "someVar + 1 + (2 + 3)"
        ]
        |> List.forall id

    let replicate count c = seq{1..count} |> Seq.fold (konst ((>>) (showChar c))) empty : ShowS

    assert
        let f n = replicate n 'a' |> show = String.replicate n "a"
        [0..10] |> List.map f |> List.forall id

module ShowList =
    type String = char list
    type ShowS = String -> String

    type IShow =
        abstract Shows : unit -> ShowS

    let showChar c = let f xs = c :: xs in f : ShowS

    let showSpace = showChar ' '

    let showString (xs: char seq) = List.append (List.ofSeq xs) : ShowS

    let empty = id : ShowS

    let show (f: ShowS) = System.String.Concat(f List.empty)

    /// Соединяет строки с помощью разделителя. Пропускает пустые элементы.
    let joins (sep: ShowS) (xs: ShowS list) =
        let rec loop acc = function
            | x:: xs ->
                let x = x []
                if List.isEmpty x then loop acc xs
                else
                    loop (acc << sep << List.append x) xs
            | [] -> acc

        let rec loop2 = function
            | x:: xs ->
                let x = x []
                if List.isEmpty x then loop2 xs
                else loop (List.append x) xs
            | [] -> id

        loop2 xs : ShowS

    let join sep = joins (showString sep)

    /// Соединяет строки с помощью разделителя.
    let joinsEmpty s: ShowS list -> ShowS =
        let join s =
            cond List.isEmpty (k empty) (List.reduce (fun x y -> x << (s << y)))
        join s
    let joinEmpty s = joinsEmpty (showString s)

    let (nl: ShowS) = showString System.Environment.NewLine
    let lines = joins nl
    let between (opened: ShowS) (closed: ShowS) (p: ShowS) = (opened << p << closed): ShowS

    let showParen =
        let l, r = showChar '(', showChar ')'
        fun b ->
            if b then between l r
            else id
            : ShowS -> ShowS

    let bet opened closed =
        between (showString opened) (showString closed)
        : _ -> ShowS

    /// show by `obj.ToString()`
    let showByToString x = showString (x.ToString()) : ShowS

    /// ## Example
    /// ```fsharp
    /// type Item =
    ///     {
    ///         Name: string
    ///         Cost: int
    ///     }
    ///
    ///     interface IShow with
    ///         member this.Shows () =
    ///             showString this.Name << nl
    ///             << showByToString this.Cost
    ///
    /// { Name = "Sword"; Cost = 100 }
    /// |> shows
    /// |> show // "Sword\n100"
    /// ```
    let shows (x: #IShow) = x.Shows()

    let showAutoParen parOpen: (_ -> ShowS) =
        let parClose = function
            | "(" -> ")"
            | "[" -> "]"
            | "{" -> "}"
            | x -> x

        bet parOpen (parClose parOpen)

    assert
        let s = showAutoParen "("
        show (s (showChar 'a') << s (showChar 'b')) = "(a)(b)"

    let replicate count c = seq{1..count} |> Seq.fold (konst ((>>) (showChar c))) empty : ShowS

    let showReplicate count (c: ShowS) =
        seq{1..count} |> Seq.fold (konst ((>>) c)) empty : ShowS

    assert
        let f n = replicate n 'a' |> show = String.replicate n "a"
        [0..10] |> List.map f |> List.forall id
