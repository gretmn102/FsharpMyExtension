namespace FsharpMyExtension

module Pair =
    let fold f (st:'State) (x, y) = f st x y
    /// `curry`
    let reduce f (x,y) = f x y
    let mapFst fn (x, y) = fn x, y
    let mapSnd fn (x, y) = x, fn y
    let mapPair f g (x, y) = f x, g y
    let mapBoth f (x, y) = f x, f y
[<AutoOpen>]
module FSharpExt =
    let rec until cond f init =
        match init with
        | curr when cond curr -> curr
        | curr -> until cond f <| f curr
    assert
        let init = 1
        let cond = (<) 100
        let fn = ((*) 2)
        until cond fn init = (Seq.unfold(fun st -> Some(st,fn st)) init |> Seq.find cond)
    assert
        let init = 1
        until (fun x -> x % 2 <> 0) ((+)1) init = init
    let rec for' fromInclusive toInclusive f st =
        if fromInclusive > toInclusive then st
        else for' (fromInclusive + 1) toInclusive f (f st fromInclusive)
    let while' f =
        // if f() then () else while' f
        let mutable x = true
        while x do
            if not (f ()) then
                x <- false
    assert
        let x = ref 0
        while' (fun () ->
            if !x < 10 then
                incr x
                true
            else false
            )
        !x = 10
    let flip f x y = f y x
    assert
        let f b s = if b then sprintf "view: %s" s else sprintf "not view"
        let s = "some text"
        let b = true
        flip f s b = f b s
    /// (<||)
    let curry = (<||)
    let uncurry f x y = f(x, y)

    let konst f x _ = f x
    let konts f _ y = f y
    // let mapFst fn (x, y) = fn x, y
    // let mapSnd fn (x, y) = x, fn y
    // let mapPair f g (x, y) = f x, g y
    // let mapBoth f (x, y) = f x, f y
    let mapFst = Pair.mapFst
    let mapSnd = Pair.mapSnd
    let mapPair = Pair.mapPair
    let mapBoth = Pair.mapBoth
    let on f g x = f x, g x

    let cond p f g x = if p x then f x else g x
    let comma x y = x, y
    let swap (x, y) = y, x

    let s = fun f g x -> f x (g x)
    let s' = fun g f x -> f x (g x)
    let b = fun f g x -> f (g x)
    assert
        let test f nul init = Option.fold (konts f) nul init = match init with None -> nul | Some x -> f x
        test ((+) 1) 0 <| Some 1

    let inline succ x = x + LanguagePrimitives.GenericOne
    let inline pred x = x - LanguagePrimitives.GenericOne
    let isEven x = x % 2 = 0
    (*
    I := λx.x
    K := λx.λy.x
    S := λx.λy.λz.x z (y z)
    B := λx.λy.λz.x (y z)
    C := λx.λy.λz.x z y
    W := λx.λy.x y y
    *)
    // konst f x y = f x
    // konst f x y = f (k x y)
    // konst f x = f << (k x)
    // konst f x = (<<) f (k x)
    // konst f = (<<) f << k

    // konst x y f = k x y |> f
    // konst x y f = (|>) (k x y) f
    // konst x y = (|>) (k x y)
    // konst x = (|>) << (k x)
    // konst x = (<<) (|>) (k x)
    // konst = ((<<) (|>)) << k

    // konst = uncurry (curry k >> f)

    let k x _ = x
    let kk _ y = y
    let w x y = x y y

    let (^|) = (<|)

    let cprintfn background foreground fmt =
        Printf.kprintf (fun x ->
            let f', b' = System.Console.ForegroundColor, System.Console.BackgroundColor
            System.Console.ForegroundColor <- foreground
            System.Console.BackgroundColor <- background
            System.Console.WriteLine x
            System.Console.ForegroundColor <- f'
            System.Console.BackgroundColor <- b'
        ) fmt
    // System.Console.BackgroundColor <- System.ConsoleColor.Black
    // System.Console.ForegroundColor <- System.ConsoleColor.Gray

module Show =
    open FSharpExt
    type String = char seq
    type ShowS = String -> String

    let split sep (str:string) = str.Split([|sep|])

    let showChar c = Seq.append (Seq.singleton c) : ShowS
    let showString xs = Seq.append xs : ShowS

    // assert
    //    let s = Seq.append (seq{printfn "eval1"; yield ();}) (seq{printfn "eval2"; yield (); printfn "eval3"; yield ();})
    //    let cons = seq{ printfn "eval1"; yield (); yield! (seq{printfn "eval2"; yield (); printfn "eval3"; yield ();}) }
    //    Seq.take 2 cons
    //    true
    let empty = id : ShowS

    let (nl:ShowS) = showString System.Environment.NewLine

    let between (opened:ShowS) (closed:ShowS) (p:ShowS) = (opened << p << closed):ShowS

    //let showParen b = (fun p -> if b then showChar '(' << p << showChar ')' else p):(ShowS -> ShowS)
    let showParen b = (cond (k b) (between (showChar '(') (showChar ')')) id):(ShowS -> ShowS)

    let bet opened closed (p:ShowS) = (showString opened << p << showString closed):ShowS
    let show (x:ShowS) = System.String.Concat(x Seq.empty)
    let shows x = (showString (x.ToString())):ShowS

    let showAutoParen parOpen : (_ -> ShowS) =
        let parClose = function
            | "(" -> ")"
            | "[" -> "]"
            | "{" -> "}"
            | x -> x
        //showString parOpen << p << showString (parClose parOpen)
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
        show (join " + " [ showString "someVar"; showChar '1'; showAutoParen "(" <| showString "2 + 3" ]) = "someVar + 1 + (2 + 3)" ] |> List.forall id

    let replicate count c = seq{1..count} |> Seq.fold (konst ((>>) (showChar c))) empty : ShowS

    assert
        let f n = replicate n 'a' |> show = String.replicate n "a"
        [0..10] |> List.map f |> List.forall id
module ShowList =
    open FSharpExt
    type String = char list
    type ShowS = String -> String

    let showChar c = let f xs = c :: xs in f : ShowS

    let showString (xs:char seq) = List.append (List.ofSeq xs) : ShowS

    let empty = id : ShowS
    let show (x:ShowS) = System.String.Concat(x List.empty)

    /// Соединяет строки с помощью разделителя. Пропускает пустые элементы.
    let joins (sep:ShowS) (xs:ShowS list) =
        let rec loop acc = function
            | x::xs ->
                let x = x []
                if List.isEmpty x then loop acc xs
                else
                    loop (acc << sep << List.append x) xs
            | [] -> acc
        let rec loop2 = function
            | x::xs ->
                let x = x []
                if List.isEmpty x then loop2 xs
                else loop (List.append x) xs
            | [] -> id
        loop2 xs : ShowS
    let join sep = joins (showString sep)

    /// Соединяет строки с помощью разделителя.
    let joinsEmpty s : ShowS list -> ShowS =
        let join s =
            cond List.isEmpty (k empty) (List.reduce (fun x y -> x << (s << y)))
        join s
    let joinEmpty s = joinsEmpty (showString s)

    let (nl:ShowS) = showString System.Environment.NewLine
    let lines = joins nl
    let between (opened:ShowS) (closed:ShowS) (p:ShowS) = (opened << p << closed):ShowS

    let showParen =
        let l, r = showChar '(', showChar ')'
        fun b ->
            if b then between l r
            else id
            : ShowS -> ShowS

    let bet opened closed =
        between (showString opened) (showString closed)
        // : _ -> ShowS

    let shows x = showString (x.ToString()) :ShowS

    let showAutoParen parOpen : (_ -> ShowS) =
        let parClose = function
            | "(" -> ")"
            | "[" -> "]"
            | "{" -> "}"
            | x -> x
        //showString parOpen << p << showString (parClose parOpen)
        bet parOpen (parClose parOpen)

    assert
        let s = showAutoParen "("
        show (s (showChar 'a') << s (showChar 'b')) = "(a)(b)"

    // let join s (xs: ShowS list) =
    //     let join s =
    //         cond List.isEmpty (k empty) (List.reduce (fun x y -> x << (s << y)))
    //     join (showString s) xs : ShowS

    // assert
    //     [
    //     join " + " [] |> show = ""
    //     join " + " [ showString "1" ] |> show = "1"
    //     join " + " [ showString "1"; showString "2" ] |> show = "1 + 2"
    //     show (join " + " [ showString "someVar"; showChar '1'; showAutoParen "(" <| showString "2 + 3" ]) = "someVar + 1 + (2 + 3)" ] |> List.forall id

    let replicate count c = seq{1..count} |> Seq.fold (konst ((>>) (showChar c))) empty : ShowS

    assert
        let f n = replicate n 'a' |> show = String.replicate n "a"
        [0..10] |> List.map f |> List.forall id
