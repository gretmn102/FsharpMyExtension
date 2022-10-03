module Parser.Primitives2
open FsharpMyExtension
open FsharpMyExtension.Either
open FsharpMyExtension.Tree

type 'Elem T =
    // `seq` как раз избавит от проблем с пожиранием памяти, хотя каждый раз применять `Seq.ofList` на поток...
    | Back of 'Elem seq * string
    | NotBack of string
    | Or
type IsChanged = bool
type Result<'Elem, 'State, 'UState> =
    ('Elem list * 'UState) * Either<IsChanged * Tree<'Elem T>, 'State>
type Pars<'Elem, 'State, 'UState> =
    'Elem list * 'UState -> Result<'Elem, 'State, 'UState>

let notChange lab : Either<IsChanged * Tree<'Elem T>, 'State> =
    Left(false, Node(NotBack lab, []) )

let pzero : Pars<_,_,_> = let fn xs = (xs, notChange "") in fn
let fail lab : Pars<_,_,_> = let fn xs = (xs, notChange lab) in fn

let (>>=) (p: Pars<'Elem,'a,'u>) (f:'a -> Pars<'Elem,'b,'u>) : Pars<'Elem,'b,'u> =
    p >>
    fun (xs, x) ->
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
            xs, Left(false, Node(Back(Seq.ofList (fst ys), "back"), [x]))
        | _ -> ys, x
    fn

let trav (p: Pars<_,Result<_,'State,_>,'UState1>) : Pars<'Elem,'State,'UState1> =
    p
    >> fun ((xs, u), res) ->
        match res with
        | Right ((xs2, u2), res) ->
            match res with
            | Right _ -> (xs, u2), res
            | Left _ -> (xs2, u2), res
        | Left x -> (xs, u), Left x

let preturn (x:'State) = let f = flip comma (Right x) in f : Pars<'Elem,'State,'u>

let eof lab =
    let fn : Pars<'Elem,unit,'u> =
        s comma
            (function
            | [],_ -> Right ()
            | _  -> notChange lab)
    in fn
let private steamEmpty2 (p: _ -> Result<_,_,'u>) lab =
    let fn = function
        | [], st -> ([], st), notChange lab
        | x::xs, u -> p ((x, xs), u)
    fn : Pars<'Elem,_,_>
let private steamEmpty (p: Pars<_,_,'u>) lab =
    let fn = function
        | [], st -> ([], st), notChange lab
        | xs -> p xs
    fn : Pars<'Elem,_,_>

let praw : Pars<'Elem,'Elem,'u> =
    let fn xs =
        xs
        |> steamEmpty (
            on (mapFst List.tail)
               (fst >> List.head >> Right)
        ) "expected any element, but stream is empty"
    fn
let satisfy f note : Pars<'Elem,'Elem,'u> =
    let fn xs =
        xs
        |> steamEmpty (
            s (fun xs ->
                cond (snd >> f)
                    (mapSnd Right)
                    (k (xs, notChange note)))
                (on (mapFst List.tail) (fst >> List.head) )
            ) (sprintf "expected '%s', but stream is empty" note)
    fn
/// Не знаю, противоречит ли оно главной концепции...
let satisfym f note : Pars<'Elem,_,'u> =
    let fn xs =
        xs
        |> steamEmpty (
            s (fun xs (ys,x) ->
                match f x with
                | Some x -> ys, Right x
                | None -> xs, notChange note
            ) (on (mapFst List.tail) (fst >> List.head))
        ) (sprintf "expected '%s', but stream is empty" note)
    fn

let satisfyRaw f note : Pars<'Elem,_,'u> =
    let fn xs =
        xs
        |> steamEmpty2
            (fun ((x, xs), u) ->
                let u, res = f (x, u)
                if Either.isRight res then
                    (xs, u), res
                else
                    (x::xs, u), res
            )
            (sprintf "expected '%s', but stream is empty" note)
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
    fn : Pars<'Elem, _, _>


let (<?>) (p:Pars<'Elem,_,_>) label : Pars<'Elem,_,_> =
    p
    >> fun (xs,res) ->
        res
        |> Either.either
            (fun _ -> xs, notChange label)
            (fun x -> xs, Right x )
let (<|>) (p1:Pars<'Elem,'State,_>) (p2:Pars<'Elem,'State,_>) =
    let fn x next =
        match x with
        | _, Left y ->
            match y with
            | true, _ -> x
            | _, y -> next y
        | x -> x
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
let tuple3 p1 p2 p3 =
    pipe3 p1 p2 p3 (fun x y z -> x, y, z)
let tuple4 p1 p2 p3 p4 =
    pipe4 p1 p2 p3 p4 (fun w x y z -> w, x, y, z)
let tuple5 p1 p2 p3 p4 p5 =
    pipe5 p1 p2 p3 p4 p5 (fun v w x y z -> v, w, x, y, z)

let many (p:Pars<'Elem,'State,_>) : Pars<'Elem,'State list,_> =
    // (p >>= fun x -> many p |>> fun xs -> x::xs ) <|> preturn []
    let rec fn acc xs =
        let (xs, r) = p xs
        match r with
        | Right x -> fn (x::acc) xs
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
let opt x = (x |>> Some) <|> (preturn None)

let createParserForwardedToRef() =
    let dummyParser = fun stream -> failwith "a parser created with createParserForwardedToRef was not initialized"
    let r = ref dummyParser
    (fun stream -> !r stream), r : Pars<_, _,'u> * Pars<_, _,'u> ref

let run s (p:Pars<_,_,_>) = (List.ofSeq s, ()) |> p
let runs s (st:'u) (p:Pars<_,_,_>) = (List.ofSeq s, st) |> p

module FParsecExt =
    open FsharpMyExtension.Either

    let runFParsec fParsecParser p =
        p .>>. getUserState
        |>> fun (x, u) ->
            match FParsec.CharParsers.runParserOnString fParsecParser u "" x with
            | FParsec.CharParsers.Success(x, u, _) -> ([], u), Right x
            | FParsec.CharParsers.Failure(x, _, u) -> ([], u), notChange x
        |> trav

    let pfloat p = runFParsec FParsec.CharParsers.pfloat p
    let pint64 p = runFParsec FParsec.CharParsers.pint64 p
    let pint32 p = runFParsec FParsec.CharParsers.pint32 p