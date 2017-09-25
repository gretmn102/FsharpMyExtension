namespace Parser

//#if INTERACTIVE
//#load "Library1.fs"
//#load "List.fs"
//#load "Either.fs"
//#endif

open FsharpMyExtension.Either
open FsharpMyExtension.FSharpExt

module Primitives2 =
    //type ElemS<'StateElem, 'Elem> =
    //    { State:'StateElem; IsEmpty:'StateElem -> bool; Head:'StateElem -> 'Elem; Tail:'StateElem -> 'StateElem }
    //let head { State = s; Head = h } = h s
    //let tail { State = s; Tail = t } = t s
    //let isEmpty { State = s; IsEmpty = f } = f s
    type T<'Elem> =
        | Back of 'Elem list * string
        | NotBack of string
        //| NotChange
    //type T2<'Elem> = T<'Elem> * string
    type Change = bool
    type Result<'Elem, 'State> = 'Elem list * Either< Change * list<T<'Elem>>, 'State>
    type Pars<'Elem, 'State> = 'Elem list -> Result<'Elem, 'State>

    // Pars<'Elem, Pars<'Elem, 'State>> -> Pars<'Elem, 'State>
    // ('Elem list -> Result<'Elem, Pars<'Elem, 'State>>) -> ('Elem list -> Result<'Elem, 'State>)
    let notChange lab = Left(false, [NotBack lab] )
    //let cat (x: Pars<'Elem, Result<_, 'State>>) = x >> Either.concat
    let pzero : Pars<_,_> = let fn xs = (xs, Left(false, [NotBack ""])) in fn
    let (>>=) (p: Pars<'Elem,'State>) (f:'State->Pars<'Elem,'State2>) : Pars<'Elem,'State2> =
        p >> fun (xs, x) -> //x |> Either.either (fun y -> xs, Left y ) (flip f xs)
            x |> Either.either (fun y -> xs, Left y )
                (flip f xs >> function xs, Left y -> xs, Left(true, snd y) | x -> x )
        //Left(true, snd y)
    let attempt (p:Pars<_,_>) : Pars<_,_> = 
        let fn xs = p xs |> fun (ys, x) -> 
            match x with
            | Left(_, x) ->
                let f x = 
                    match x with
                    | Back _ as x -> x
                    | NotBack m -> Back(ys, m)
                xs, Left(false, [ f (List.head x) ])
            | _ -> ys, x
        fn
    let trav (x: Pars<_, Result<_, 'State>>) : Pars<'Elem, 'State>  =
        x >> mapSnd (Either.bind snd)

    let preturn (x:'State) = let f = flip comma (Right x) in f : Pars<'Elem,'State>
    
    let pend lab = 
        let fn : Pars<'Elem,unit> =
            s comma
                (function
                | [] -> Right ()
                | _ -> notChange lab)
            //|> flip comma xs
        in fn
    let steamEmpty (f:Pars<'Elem,_>) lab =
        let fn = function
            | [] -> [], notChange lab
            | xs -> f xs in fn : Pars<'Elem,_>
    
    let praw : Pars<'Elem,'Elem> =
        let fn xs = xs |> steamEmpty (on List.tail (List.head >> Right)) "stream is empty"
        fn
    let satisfy f note : Pars<'Elem, 'Elem> =
        let fn xs =
            xs |> steamEmpty (
                s (fun xs ->
                    cond (snd >> f)
                        (mapSnd Right)
                        (k (xs, notChange note)))
                    (on List.tail List.head) ) "stream is empty"
        fn

    let (<?>) (p:Pars<'Elem,_>) label : Pars<'Elem,_> =
        p >> fun (xs,res) -> res |> Either.either (fun _ -> xs, notChange label) (fun x -> xs, Right x )
    let (<|>) (p1:Pars<'Elem,'State>) (p2:Pars<'Elem,'State>) = 
        // let fn xs : Pars<'Elem,'State> =
            // match p1 xs with
            // | xs, Left _ -> failwith ""
        let fn x next = match x with _, Left y -> (match y with true, _ -> x | _, y -> next y) | x -> x
        let fn xs =
            fn (p1 xs) (fun x ->
            fn (p2 xs) (fun y ->
            xs, Left(false, List.concat [x; y])))
        fn : Pars<'Elem,'State> 
    let (>>.) p1 p2 = p1 >>= k p2
    let (>>%) p x = p >>= k (preturn x)
    let (.>>) p1 p2 = p1 >>= (>>%) p2
    let (.>>.) p1 p2 = p1 >>= fun x -> p2 >>= fun y -> preturn (x, y)
    let (|>>) p f = p >>= (f >> preturn)
    let pipe2 p1 p2 f = p1 >>= fun a -> p2 >>= fun b -> preturn (f a b)
    let pipe3 p1 p2 p3 f = pipe2 p1 p2 comma >>= ((|>>) p3 << curry f)
    let pipe4 p1 p2 p3 p4 f = pipe3 p1 p2 p3 ((<<) comma << comma) >>= ((|>>) p4 << curry (curry f))
    let pipe5 p1 p2 p3 p4 p5 f = pipe4 p1 p2 p3 p4 ((<<) ((<<) comma << comma) << comma) >>= (*fun (((a, b), c), d) -> p5 |>> f a b c d *) ((|>>) p5 << curry (curry (curry f)))
    let rec many (p:Pars<'Elem,'State>) : Pars<'Elem,'State list> = 
        //(p >>= fun x -> many p |>> fun xs -> x::xs ) <|> preturn []
        let rec fn acc xs = 
            match p xs with
            | xs, Right x -> fn (x::acc) xs
            | _, Left(false, _) ->
                xs, Right(List.rev acc)
            | xs, (Left(true, ys) as x) ->
                xs, Left(true, ys)
        fn []
    let many1 p = pipe2 p (many p) (fun hd tl -> hd::tl)
    let sepBy p sep = pipe2 p (many (sep >>. p) ) (fun hd tl -> hd::tl) <|> (preturn [])

    let run s (p:Pars<_,_>) = List.ofSeq s |> p





