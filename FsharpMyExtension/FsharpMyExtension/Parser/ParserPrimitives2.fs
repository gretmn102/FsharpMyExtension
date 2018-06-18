module Parser.Primitives2

//#if INTERACTIVE
//#load "Library1.fs"
//#load "List.fs"
//#load "Either.fs"
//#endif


open FsharpMyExtension
open FsharpMyExtension.Either
open FsharpMyExtension.Tree
open FsharpMyExtension.FSharpExt
type T<'Elem> =
    // а как на счет этого
    // | Back of 'Elem seq * string
    // `seq` как раз избавит от проблем с пожиранием памяти.
    // Хотя, каждый раз `Seq.ofList` на поток, хм...
    | Back of 'Elem seq * string
    | NotBack of string
    | Or
    //| NotChangeF
//type T2<'Elem> = T<'Elem> * string
type Change = bool
type Result<'Elem, 'State, 'UState> = ('Elem list * 'UState) * Either<Change * Tree<T<'Elem>>, 'State>
type Pars<'Elem, 'State, 'UState> = 'Elem list * 'UState -> Result<'Elem, 'State, 'UState>

// Pars<'Elem, Pars<'Elem, 'State>> -> Pars<'Elem, 'State>
// ('Elem list -> Result<'Elem, Pars<'Elem, 'State>>) -> ('Elem list -> Result<'Elem, 'State>)

let notChange lab = Left(false, Node(NotBack lab, []) )
//let cat (x: Pars<'Elem, Result<_, 'State>>) = x >> Either.concat
let pzero : Pars<_,_,_> = let fn xs = (xs, notChange "") in fn
let (>>=) (p: Pars<'Elem,'a,'u>) (f:'a -> Pars<'Elem,'b, 'u>) : Pars<'Elem,'b, 'u> =
    p >> fun (xs, x) -> //x |> Either.either (fun y -> xs, Left y ) (flip f xs)
        x
        |> Either.either (fun y -> xs, Left y )
                (fun st ->
                    match f st xs with
                    | xs, Left y -> xs, Left(true, snd y)
                    | x -> x)

let attempt (p:Pars<_,_,'u>) : Pars<_,_,_> = 
    let fn xs =
        let ys, x = p xs
        match x with
        | Left(_, x) ->
            // let f x = 
                // match x with
                // | Back _ as x -> x
                // | NotBack m -> Back(ys, m)
            xs, Left(false, Node(Back(Seq.ofList (fst ys), "back"), [x]) (* [ f (List.head x) ] *))
        | _ -> ys, x
    fn
let trav (x: Pars<_, Result<_, 'State, _>, 'u>) : Pars<'Elem, 'State, _>  =
    x >> mapSnd (Either.bind snd)

let preturn (x:'State) = let f = flip comma (Right x) in f : Pars<'Elem,'State, 'u>

let pend lab = 
    let fn : Pars<'Elem,unit,'u> =
        s comma
            (function
            | [],_ -> Right ()
            | _  -> notChange lab)
        //|> flip comma xs
    in fn
let steamEmpty (f: Pars<_,_,'u>) lab =
    let fn = function
        | [],st -> ([],st), notChange lab
        | xs -> f xs
    fn : Pars<'Elem,_,_>

let praw : Pars<'Elem,'Elem, 'u> =
    let fn xs =
        xs
        |> steamEmpty (
            on (mapFst List.tail)
               (fst >> List.head >> Right)) "expected any element, but stream is empty"
    fn
let satisfy f note : Pars<'Elem, 'Elem, 'u> =
    let fn xs =
        xs |> steamEmpty (
            s (fun xs ->
                cond (snd >> f)
                    (mapSnd Right)
                    (k (xs, notChange note)))
                (on (mapFst List.tail) (fst >> List.head) ) ) (sprintf "expected '%s', but stream is empty" note)
    fn
/// Не знаю, противоречит ли оно главной концепции...

let satisfym f note : Pars<'Elem, _, 'u> =
    let fn xs =
        xs |> steamEmpty (
            s (fun xs (ys,x) ->
                match f x with
                | Some x -> ys, Right x
                | None -> xs, notChange note
                )
              (on (mapFst List.tail) (fst >> List.head) ) ) (sprintf "expected '%s', but stream is empty" note)
    fn

let getUserState =
    let fn x = x, Right(snd x)
    fn : Pars<'Elem,'u,'u>
let setUserState (st:'u) =
    let fn x = (fst x, st), Right ()
    fn : Pars<'Elem,_,_>
let updateUserState (f:'u -> 'u) =
    let fn (xs,st) = (xs, f st), Right ()
    fn : Pars<'Elem,_,_>
let userStateSatisfies (f:'u -> bool) =
    let fn (xs,st) =
        let res =
            if f st then Right ()
            else notChange "userStateSatisfies error"
        (xs, st), res
    fn : Pars<'Elem,_,_>


let (<?>) (p:Pars<'Elem,_,_>) label : Pars<'Elem,_,_> =
    p
    >> fun (xs,res) ->
        res |> Either.either (fun _ -> xs, notChange label) (fun x -> xs, Right x )
let (<|>) (p1:Pars<'Elem,'State,_>) (p2:Pars<'Elem,'State,_>) = 
    // let fn xs : Pars<'Elem,'State> =
        // match p1 xs with
        // | xs, Left _ -> failwith ""
    let fn x next = match x with _, Left y -> (match y with true, _ -> x | _, y -> next y) | x -> x
    let fn xs =
        fn (p1 xs) (fun x ->
        fn (p2 xs) (fun y ->
        xs, Left(false, Node(Or, [x; y]))))
    fn : Pars<'Elem,'State, _>
let (>>.) p1 p2 = p1 >>= k p2
let (>>?) p1 p2 = attempt (p1 >>. p2)
let (>>%) p x = p >>= k (preturn x)
let (.>>) p1 p2 = p1 >>= (>>%) p2
let (.>>?) p1 p2 = attempt (p1 .>> p2)
let (.>>.) p1 p2 = p1 >>= fun x -> p2 >>= fun y -> preturn (x, y)
let (|>>) p f = p >>= (f >> preturn)
let pipe2 p1 p2 f = p1 >>= fun a -> p2 >>= fun b -> preturn (f a b)
let pipe3 p1 p2 p3 f = pipe2 p1 p2 comma >>= ((|>>) p3 << curry f)
let pipe4 p1 p2 p3 p4 f = pipe3 p1 p2 p3 ((<<) comma << comma) >>= ((|>>) p4 << curry (curry f))
let pipe5 p1 p2 p3 p4 p5 f = pipe4 p1 p2 p3 p4 ((<<) ((<<) comma << comma) << comma) >>= (*fun (((a, b), c), d) -> p5 |>> f a b c d *) ((|>>) p5 << curry (curry (curry f)))
let rec many (p:Pars<'Elem,'State,_>) : Pars<'Elem,'State list,_> = 
    //(p >>= fun x -> many p |>> fun xs -> x::xs ) <|> preturn []
    let rec fn acc xs = 
        let (xs, r) = p xs
(*             r |> Either.either (
                cond fst Left (k (Right(List.rev acc))) >> comma xs
                // (if change then Left(true, ys) else Right(List.rev acc)) |> comma xs
                )
            (fun x -> fn (x::acc) xs) *)
            
        match r with
        | Right x -> fn (x::acc) xs
        // | _, Left(false, _) ->
        //     xs, Right(List.rev acc)
        // | xs, (Left(true, ys) as x) ->
        //     xs, Left(true, ys)
        | Left(changed, ys) ->
            if changed then Left(true, ys)
            else Right(List.rev acc)
            |> fun x -> xs, x
    fn []
let many1 p = pipe2 p (many p) (fun hd tl -> hd::tl)
let sepBy p sep =
    pipe2 p (many (attempt(sep >>. p)) ) (fun hd tl -> hd::tl) <|> (preturn [])
/// `Seq.reduce (<|>) xs` - optimise?
let choice xs = Seq.reduce (<|>) xs

let run s (p:Pars<_,_,_>) = (List.ofSeq s, ()) |> p
let runs s (st:'u) (p:Pars<_,_,_>) = (List.ofSeq s, st) |> p





