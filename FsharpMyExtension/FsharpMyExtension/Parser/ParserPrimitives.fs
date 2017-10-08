module Parser.Primitives


open FsharpMyExtension.Either
open FsharpMyExtension.FSharpExt
//type ElemS<'StateElem, 'Elem> =
//    { State:'StateElem; IsEmpty:'StateElem -> bool; Head:'StateElem -> 'Elem; Tail:'StateElem -> 'StateElem }
//let head { State = s; Head = h } = h s
//let tail { State = s; Tail = t } = t s
//let isEmpty { State = s; IsEmpty = f } = f s

type Result<'Elem, 'State> = Either<string, 'State * 'Elem list>
type Pars<'Elem, 'State> = 'Elem list -> Result<'Elem, 'State>

// Pars<'Elem, Pars<'Elem, 'State>> -> Pars<'Elem, 'State>
// ('Elem list -> Result<'Elem, Pars<'Elem, 'State>>) -> ('Elem list -> Result<'Elem, 'State>)

//let cat (x: Pars<'Elem, Result<_, 'State>>) = x >> Either.concat
let trav (x: Pars<_, Result<_, 'State>>) =
    //x >> Either.bind (fun (x, y) -> x |> Either.map (mapSnd (k y))) : Pars<'Elem, 'State>
    x >> Either.bind (curry <| flip (Either.map ^< mapSnd ^< k) ) : Pars<'Elem, 'State>
let preturn (x:'State) = let f xs = Right(x, xs) in f : Pars<'Elem,'State>
let pend lab = 
    let fn = function
        | [] -> Right((),[])
        | xs -> Left <| sprintf "expected empty steam, but take: %A" (lab xs)
    in fn : Pars<'Elem,_>
let praw =
    let fn = function
        | [] -> Left "expected one or more nodes"
        | h::t -> Right(h, t)
    in fn : Pars<'Elem,_>
let satisfy f note noteExpected = 
    let fn = function
        | [] -> Left <| sprintf "expected:\n%s\nactual: stream is empty" noteExpected
        | h::t -> if f h then Right(h, t) else Left <| sprintf "expected:\n%s\nactual:\n%s" noteExpected (note h)
    fn : Pars<'Elem,'Elem>
let rec many (p:Pars<'Elem,'State>) : Pars<'Elem,'State list> = 
    let rec fn acc xs = 
        match p xs with
        | Right(x, xs) -> fn (x::acc) xs
        | Left _ -> List.rev acc, xs
    fn [] >> Right
let (<|>) (p1:Pars<'Elem,'State>) (p2:Pars<'Elem,'State>) = 
    let fn x next = match x with Left x -> next x | x -> x
    let fn xs =
        fn (p1 xs) (fun x ->
        fn (p2 xs) (fun y ->
        Left <| sprintf "%A" [x;y]))
    fn : Pars<'Elem,'State>

let (>>=) (p: Pars<'Elem,'State>) (f:'State->Pars<'Elem,'State2>) =
    p >> Either.bind (curry f) : Pars<'Elem,'State2>

let (>>.) p1 p2 = p1 >>= k p2
let (>>%) p x = p >>= k (preturn x)
let (.>>) p1 p2 = p1 >>= (>>%) p2
let (|>>) p f = p >>= (f >> preturn)

let pipe2 p1 p2 f = p1 >>= fun a -> p2 >>= fun b -> preturn (f a b)
let pipe3 p1 p2 p3 f = pipe2 p1 p2 comma >>= ((|>>) p3 << curry f)
let pipe4 p1 p2 p3 p4 f = pipe3 p1 p2 p3 ((<<) comma << comma) >>= ((|>>) p4 << curry (curry f))
let pipe5 p1 p2 p3 p4 p5 f = pipe4 p1 p2 p3 p4 ((<<) ((<<) comma << comma) << comma) >>= (*fun (((a, b), c), d) -> p5 |>> f a b c d *) ((|>>) p5 << curry (curry (curry f)))
let many1 p = pipe2 p (many p) (fun hd tl -> hd::tl)
let sepBy p sep = pipe2 p (many (sep >>. p)) (fun hd tl -> hd::tl) <|> (preturn [])

let run s (p:Pars<_,_>) = List.ofSeq s |> p