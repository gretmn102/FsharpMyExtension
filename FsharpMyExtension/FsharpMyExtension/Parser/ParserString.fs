namespace Parser

module ParserString =
    // #if INTERACTIVE
    // #load "ParserPrimitives.fs"
    // //#load "List.fs"
    // //#load "Either.fs"
    // #endif
    open FsharpMyExtension.Either
    open FsharpMyExtension.List
    open FsharpMyExtension.FSharpExt
    open FsharpMyExtension.String

    open Parser.Primitives

    //
    let pchar c = satisfy ((=) c) (sprintf "%c") (string c)

    let manySatisfy (f:char->_) = many (satisfy f (fun _ -> null) null) |>> System.String.Concat
    let manyStrings (sp:Pars<_,string>) = many sp |>> System.String.Concat
    let manyChars (cp:Pars<_,char>) = many cp |>> System.String.Concat

    let pstr =
        let chars = Set.union <| set['a'..'z'] <| set['A'..'Z']
        many1 (satisfy (chars |> fun xs c -> Set.contains c xs) (sprintf "%c") "['a'..'z']")
        |>> System.String.Concat

    let pstringCI x =
        let rec fn = function
            | t, [] -> Right((), t)
            | h::t, h'::t' -> if h = h' then fn (t, t') else Left <| sprintf "expected '%s', but actual '%c'" x h
            | [], _ -> Left <| sprintf "expected '%s', but actual stream < length word" x
        function [] -> Left <| sprintf "expected '%s', but actual null stream" x | xs -> fn (xs, List.ofSeq x)
        : Pars<_,_>
    assert
        let p s = List.ofSeq >> pstringCI s
        [p "a" "" = Left "expected 'a', but actual null stream"
         p "a" "b" = Left "expected 'a', but actual 'b'"
         p "a" "a" = Right ((), [])
         p "" "a" = Right ((), ['a'])] |> List.forall id

    let pstring x =
        let x = String.toLower x
        let rec fn = function
            | t, [] -> Right((), t)
            | h::t, h'::t' -> if System.Char.ToLower h = h' then fn (t, t') else Left <| sprintf "expected '%s', but actual '%c'" x h
            | [], _ -> Left <| sprintf "expected '%s', but actual stream < length word" x
        function [] -> Left <| sprintf "expected '%s', but actual null stream" x | xs -> fn (xs, List.ofSeq x)
        : Pars<_,_>
    assert
        let p s = List.ofSeq >> pstring s
        [p "a" "" = Left "expected 'a', but actual null stream"
         p "a" "b" = Left "expected 'a', but actual 'b'"
         p "a" "a" = Right ((), [])
         p "a" "A" = Right ((), [])
         p "A" "a" = Right ((), [])
         p "" "a" = Right ((), ['a'])] |> List.forall id  

    let pint32 =
        let len = System.Int32.MaxValue |> string |> Seq.length
        let digit = seq{'0'..'9'} |> Seq.mapi (fun i x -> x, i) |> Map.ofSeq
        let f =
            Seq.unfold (function
                | (h:char)::t ->
                    Map.tryFind h digit
                    |> Option.map (fun x -> (x, t), t)
                | [] -> None)
            >> Seq.truncate len
            >> Seq.fold (fun st (x, xs) -> fst st * 10 + x, xs) (0,[])
        let err s = Left (sprintf "expected [-](0|1|..|9) but take: %s" s)
        function
            | ['-'] -> err "empty stream"
            | '-'::((x::_) as xs) as ys ->
                if Map.containsKey x digit then
                    mapFst ((*) -1) (f xs) |> Right
                else
                    err (sprintf "%A" <| List.truncate 10 ys)
            | h::_ as xs ->
                if Map.containsKey h digit then
                    f xs |> Right
                else
                    err (sprintf "%A" <| List.truncate 10 xs)
                // let bind x next =
                //     if Map.containsKey x digit then next ()
                //     else err (List.truncate 10 xs)
                        
                // bind h (fun () ->
                //     if h = '-' then
                //         if List.isEmpty t then err
                //         else
                //             bind (List.head t) (fun () ->
                //                 mapFst ((*) -1) (f t) |> Right)
                //     else f xs |> Right
                //     )
            | [] -> Left "empty stream"
        : Pars<_,_>


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
    let isDigit x = x >= '0' && x <= '9'

    assert
        let pred (x:char) = char (int x - 1)
        [
         ['0'..'9'] |> List.map isDigit |> List.forall id
         not (pred '0' |> isDigit)
         not (succ '9' |> isDigit) ] |> List.forall id
    let isAnyOf xs = Set.ofSeq xs |> flip Set.contains
    let isNoneOf xs = not << isAnyOf xs