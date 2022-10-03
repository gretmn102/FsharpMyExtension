module Parser.FixedParser

open FsharpMyExtension
open FsharpMyExtension.Either
open FsharpMyExtension.Tree

type T =
    | Back of string
    | NotBack of string
    | Or
type IsChanged = bool
type Result<'Node, 'State, 'UState> =
    ('Node * 'UState) * Either<IsChanged * Tree<T>, 'State>
type FixedParser<'Node, 'State, 'UState> =
    ('Node * 'UState) -> Result<'Node, 'State, 'UState>

let notChange lab : Either<IsChanged * Tree<T>, 'State> =
    Left(false, Node(NotBack lab, []) )

let pzero : FixedParser<'Node, _,_> = let fn xs = (xs, notChange "") in fn
let fail lab : FixedParser<'Node, _,_> = let fn xs = (xs, notChange lab) in fn

let (>>=) (p: FixedParser<'Node,'a,'u>) (f:'a -> FixedParser<'Node,'b,'u>) : FixedParser<'Node, 'b,'u> =
    p
    >> fun (xs, x) ->
        x
        |> Either.either (fun y -> xs, Left y)
            (fun st ->
                match f st xs with
                | xs, Left y -> xs, Left(true, snd y)
                | x -> x)

let attempt (p:FixedParser<'Node,_,'u>) : FixedParser<'Node,_,_> =
    let fn xs =
        let ys, x = p xs
        match x with
        | Left(_, x) ->
            xs, Left(false, Node(Back "back", [x]))
        | _ -> ys, x
    fn

let trav (p: FixedParser<'Node,Result<'Node,'State,_>,'UState1>) : FixedParser<'Node,'State,'UState1> =
    p
    >> fun (u, res) ->
        match res with
        | Right (u2, res) ->
            match res with
            | Right _ -> u2, res
            | Left _ -> u2, res
        | Left x -> u, Left x

let preturn (x:'State) =
    let f = flip comma (Right x)
    f : FixedParser<'Node, 'State,'u>

let satisfy f note : FixedParser<'Node,_,'u> =
    let fn xs =
        let (node, u) = xs
        if f node then
            xs, Right ()
        else
            xs, notChange note
    fn

let satisfym f note : FixedParser<'Node,_,'u> =
    let fn xs =
        let node, u = xs
        match f node with
        | Some res -> (node, u), Right res
        | None -> xs, notChange note
    fn
let satisfyRaw f : FixedParser<'Node,_,'u> =
    let fn xs =
        let node, u = xs
        let u, res = f (node, u)
        if Either.isRight res then
            (node, u), res
        else
            xs, res
    fn

let getUserState =
    let fn x = x, Right(snd x)
    fn : FixedParser<'Node, 'u,'u>
let setUserState (st:'u) =
    let fn x = (fst x, st), Right ()
    fn : FixedParser<'Node, _,_>
let updateUserState (f:'u -> 'u) =
    let fn (xs,st) = (xs, f st), Right ()
    fn : FixedParser<'Node, _,_>
let userStateSatisfies (f:'u -> bool) =
    let fn (xs,st) =
        let res =
            if f st then Right ()
            else notChange "userStateSatisfies error"
        (xs, st), res
    fn : FixedParser<'Node, _, _>

let (<?>) (p:FixedParser<'Node,_,_>) label : FixedParser<'Node,_,_> =
    p
    >> fun (xs,res) ->
        res
        |> Either.either
            (fun _ -> xs, notChange label)
            (fun x -> xs, Right x )
let (<|>) (p1:FixedParser<'Node,'State,_>) (p2:FixedParser<'Node,'State,_>) =
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
    fn : FixedParser<'Node, 'State, _>
let (>>.) p1 p2 = p1 >>= k p2
let (>>?) p1 p2 = attempt (p1 >>. p2)
let (>>%) p x = p >>= k (preturn x)
let (.>>) p1 p2 = p1 >>= (>>%) p2
let (.>>?) p1 p2 = attempt (p1 .>> p2)
let (.>>.) p1 p2 = p1 >>= fun x -> p2 >>= fun y -> preturn (x, y)
let (|>>) p f = p >>= (f >> preturn)
let pipe2 p1 p2 f =
    p1 >>= fun a -> p2 >>= fun b -> preturn (f a b)
let pipe3 p1 p2 p3 f =
    pipe2 p1 p2 comma >>= ((|>>) p3 << curry f)
let pipe4 p1 p2 p3 p4 f =
    pipe3 p1 p2 p3 ((<<) comma << comma) >>= ((|>>) p4 << curry (curry f))
let pipe5 p1 p2 p3 p4 p5 f =
    pipe4 p1 p2 p3 p4 ((<<) ((<<) comma << comma) << comma) >>= (*fun (((a, b), c), d) -> p5 |>> f a b c d *) ((|>>) p5 << curry (curry (curry f)))
let tuple3 p1 p2 p3 =
    pipe3 p1 p2 p3 (fun x y z -> x, y, z)
let tuple4 p1 p2 p3 p4 =
    pipe4 p1 p2 p3 p4 (fun w x y z -> w, x, y, z)
let tuple5 p1 p2 p3 p4 p5 =
    pipe5 p1 p2 p3 p4 p5 (fun v w x y z -> v, w, x, y, z)

/// `Seq.reduce (<|>) xs` - optimize?
let choice xs = Seq.reduce (<|>) xs
let opt x = (x |>> Some) <|> (preturn None)

let createParserForwardedToRef() =
    let dummyParser = fun stream -> failwith "a parser created with createParserForwardedToRef was not initialized"
    let r = ref dummyParser
    (fun stream -> !r stream), r : FixedParser<'Node, _,'u> * FixedParser<'Node, _,'u> ref

let run node (p:FixedParser<'Node, _,_>) = (node, ()) |> p
let runs node (st:'u) (p:FixedParser<'Node, _,_>) = (node, st) |> p

let ofLinearParser get p =
    satisfyRaw (fun (node, u) ->
        let (_, (u:'u)), res =
            Primitives.runs (get node) u p

        match res with
        | Right x ->
            Right x
        | Left (isChanged, tree) ->
            let res =
                tree
                |> Tree.Tree.map (function
                    | Primitives.NotBack x -> NotBack x
                    | Primitives.Or -> Or
                    | Primitives.Back(xs, x) -> // TODO: что делать с `xs`?
                        Back x
                )
            Left(isChanged, res)
        |> fun x -> u, x
    )

let toLinearParser p =
    Primitives.satisfyRaw (fun (node, u) ->
        let (_, (u:'u)), res =
            runs node u p

        match res with
        | Right x -> Right x
        | Left (isChanged, tree) ->
            let res =
                tree
                |> Tree.map (function
                    | NotBack x -> Primitives.NotBack x
                    | Or -> Primitives.Or
                    | Back x ->
                        Primitives.Back (Seq.empty, x)
                )
            Left(isChanged, res)
        |> fun x -> u, x
    ) "eof"

let ofFParsec fparsecParser p =
    p
    >>= fun x ->
        satisfyRaw (fun (node, u) ->
            match FParsec.CharParsers.runParserOnString fparsecParser u "" x with
            | FParsec.CharParsers.Success(res, u, _) -> u, Right res
            | FParsec.CharParsers.Failure(errMsg, _, u) -> u, notChange errMsg
        )
