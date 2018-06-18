﻿namespace FsharpMyExtension

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
    let rec for' n k f st =
        if n > k then st
        else for' (n+1) k f (f st n)

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
    let b = fun f g x -> f (g x)
    assert
        let test f nul init = Option.fold (konts f) nul init = match init with None -> nul | Some x -> f x
        test ((+) 1) 0 <| Some 1

    let inline succ x = LanguagePrimitives.GenericOne + x
    let inline pred x = LanguagePrimitives.GenericOne - x
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

    let (^<) = (<<)
    let (^|) = (<|)

    // open FSharpExt

module Show =
    open FSharpExt
    type String = char seq
    type ShowS = String -> String

    let split sep (str:string) = str.Split([|sep|])

    let showChar c = Seq.append (Seq.singleton c) : ShowS
    let showString xs = Seq.append xs : ShowS

//    assert
//        let s = Seq.append (seq{printfn "eval1"; yield ();}) (seq{printfn "eval2"; yield (); printfn "eval3"; yield ();})
//        let cons = seq{ printfn "eval1"; yield (); yield! (seq{printfn "eval2"; yield (); printfn "eval3"; yield ();}) }
//        
//        Seq.take 2 cons
//        true
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

    let join s (xs: ShowS list) = 
        let join s =
            cond List.isEmpty (k empty) (List.reduce (fun x y -> x << (s << y)))
        join (showString s) xs : ShowS
        
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

    let split sep (str:string) = str.Split([|sep|])

    let showChar c = let f xs = c :: xs in f : ShowS

    let showString (xs:char seq) = List.append (List.ofSeq xs) : ShowS

    let empty = id : ShowS

    /// Это все замечательно конечно, но как мне получить:
    /// joins (showChar '\n') [showChar 'a'; showString ""; showChar 'b'] -> "a\n\nb"
    /// Конечно я совсем не против получить из этого:
    /// joins (showChar '\n') [showChar 'a'; id; showChar 'b'] -> "a\nb"
    /// Но как мне получить пустую строку?
    /// Кто ж виноват, что 'id' здесь тоже самое что и showString ""?
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
    assert
        joins (showString ", ") [id;id;showString "Abram";showString "Lyouis";id;id;showString "Loid";id;id] []
        |> System.String.Concat = "Abram, Lyouis, Loid"
    let join sep xs = joins (showString sep) xs
    let (nl:ShowS) = showString System.Environment.NewLine
    let lines = joins nl
    let between (opened:ShowS) (closed:ShowS) (p:ShowS) = (opened << p << closed):ShowS
    
    //let showParen b = (fun p -> if b then showChar '(' << p << showChar ')' else p):(ShowS -> ShowS)
    let showParen b = (cond (k b) (between (showChar '(') (showChar ')')) id):(ShowS -> ShowS)
    
    let bet opened closed (p:ShowS) = (showString opened << p << showString closed):ShowS
    let show (x:ShowS) = System.String.Concat(x List.empty)
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
