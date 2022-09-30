namespace Parser

module ParserString =
    open FsharpMyExtension
    open FsharpMyExtension.Either
    open Parser.Primitives2

    type Parser<'T, 'UserState> = Pars<char, 'T, 'UserState>

    let isDigit x = x >= '0' && x <= '9'

    let isAnyOf xs = Set.ofSeq xs |> flip Set.contains

    let isNoneOf xs = not << isAnyOf xs

    let pchar (c: char): Parser<_, 'UserState> =
        satisfy ((=) c) (sprintf "expected '%c'" c)

    let pcharCI (c: char): Parser<_, 'UserState> =
        let c = System.Char.ToLower c
        satisfy (System.Char.ToLower >> (=) c) (sprintf "expected '%c' case ignore" c)

    let manySatisfy (f: char -> _): Parser<_, 'UserState> =
        many (satisfy f "Unknown Error(s)") |>> System.String.Concat

    let manyStrings (stringParser: Parser<string, 'UserState>): Parser<_, 'UserState> =
        many stringParser |>> System.String.Concat

    let manyChars (charParser: Parser<char, 'UserState>): Parser<_, 'UserState> =
        many charParser |>> System.String.Concat

    let pstr<'UserState> : Parser<_, 'UserState> =
        let chars = Set.union (set['a'..'z']) (set['A'..'Z'])
        many1 (satisfy (fun c -> Set.contains c chars) "expected ['a'..'z'] | ['A'..'Z']" )
        |>> System.String.Concat

    let pstringCI (str: string): Parser<_, 'UserState> =
        if str = "" then failwith "the string is empty"

        let p: Parser<_, 'UserState> =
            let rec f acc i =
                if i < str.Length then
                    f (acc >>. pcharCI str.[i]) (i + 1)
                else
                    acc
            f (pcharCI str.[0]) 1

        p <?> (sprintf "expected '%s' case ignore" str)
        >>% str

    let pstring (str: string): Parser<_, 'UserState> =
        if str = "" then failwith "the string is empty"

        let p: Parser<_, 'UserState> =
            let rec f acc i =
                if i < str.Length then
                    f (acc >>. pchar str.[i]) (i + 1)
                else
                    acc
            f (pchar str.[0]) 1

        p <?> (sprintf "expected '%s'" str)
        >>% str

    // todo
    // let pint32 =
    //     let len = System.Int32.MaxValue |> string |> Seq.length
    //     let digit = seq{'0'..'9'} |> Seq.mapi (fun i x -> x, i) |> Map.ofSeq
    //     let f =
    //         Seq.unfold (function
    //             | (h:char)::t ->
    //                 Map.tryFind h digit
    //                 |> Option.map (fun x -> (x, t), t)
    //             | [] -> None)
    //         >> Seq.truncate len
    //         >> Seq.fold (fun st (x, xs) -> fst st * 10 + x, xs) (0,[])
    //     let err s = Left (sprintf "expected [-](0|1|..|9) but take: %s" s)
    //     function
    //         | ['-'] -> err "empty stream"
    //         | '-'::((x::_) as xs) as ys ->
    //             if Map.containsKey x digit then
    //                 mapFst ((*) -1) (f xs) |> Right
    //             else
    //                 err (sprintf "%A" <| List.truncate 10 ys)
    //         | h::_ as xs ->
    //             if Map.containsKey h digit then
    //                 f xs |> Right
    //             else
    //                 err (sprintf "%A" <| List.truncate 10 xs)
    //             // let bind x next =
    //             //     if Map.containsKey x digit then next ()
    //             //     else err (List.truncate 10 xs)

    //             // bind h (fun () ->
    //             //     if h = '-' then
    //             //         if List.isEmpty t then err
    //             //         else
    //             //             bind (List.head t) (fun () ->
    //             //                 mapFst ((*) -1) (f t) |> Right)
    //             //     else f xs |> Right
    //             //     )
    //         | [] -> Left "empty stream"
    //     : Pars<_,_>

    // todo
    // let pfloat32 =
    //     let len = System.Single.MaxValue |> string |> Seq.length
    //     let digit = seq{'0'..'9'} |> Seq.mapi (fun i x -> x, i) |> Map.ofSeq
    //     let f =
    //         Seq.unfold (function (h:char)::t -> Map.tryFind h digit |> Option.map (fun x -> (x, t), t) | [] -> None)
    //         >> Seq.truncate len
    //         >> Seq.fold (fun st (x, xs) -> fst st * 10 + x, xs) (0,[])
    //     function
    //         | h::t as xs -> (if h = '-' then mapFst ((*) -1) (f t) else f xs) |> Right
    //         | [] -> Left ""
    //     : Pars<_,_>
