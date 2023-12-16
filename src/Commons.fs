namespace FsharpMyExtension

[<AutoOpen>]
module Commons =
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
    let on f g x = Pair.on f g x

    let cond p f g x = if p x then f x else g x
    let comma x y = Pair.create x y
    let swap pair = Pair.swap pair

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

    #if !FABLE_COMPILER
    let cprintfn background foreground fmt =
        Printf.kprintf (fun x ->
            let f', b' = System.Console.ForegroundColor, System.Console.BackgroundColor
            System.Console.ForegroundColor <- foreground
            System.Console.BackgroundColor <- background
            System.Console.WriteLine x
            System.Console.ForegroundColor <- f'
            System.Console.BackgroundColor <- b'
        ) fmt
    #endif

    type PipeBackwardBuilder() =
        member __.Bind (f, next) =
            f next

        member __.Return x =
            x

    /// ```fsharp
    /// let print str next =
    ///     printf "%s" str
    ///     next ()
    ///
    /// let readLine () next =
    ///     System.Console.ReadLine ()
    ///     |> next
    ///
    /// let withoutBuilder () =
    ///     print "Input your name: " <| fun () ->
    ///     readLine () <| fun name ->
    ///     print (sprintf "Your name is %s" name) <| fun () ->
    ///     ()
    ///
    /// let withBuilder () =
    ///     pipeBackwardBuilder {
    ///         do! print "Input your name: "
    ///         let! name = readLine ()
    ///         do! print (sprintf "Your name is %s" name)
    ///     }
    /// ```
    let pipeBackwardBuilder = PipeBackwardBuilder()

    /// ## Example
    /// ```fsharp
    /// type Item =
    ///     {
    ///         Name: string
    ///         Cost: int
    ///     }
    ///     static member Deserialize str =
    ///         let p =
    ///             pipe2
    ///                 (manySatisfy ((<>) '\n') .>> newline)
    ///                 pint32
    ///                 (fun name cost ->
    ///                     {
    ///                         Name = name
    ///                         Cost = cost
    ///                     }
    ///                 )
    ///
    ///         match run p str with
    ///         | Success(res, _, _) -> Result.Ok res
    ///         | Failure(errMsg, _, _) -> Result.Error errMsg
    ///
    /// ((deserialize "Sword\n300") : Result<Item, string>)
    /// // Result.Ok { Name = "Sword"; Cost = 300 }
    /// ```
    let inline deserialize<'T when 'T : (static member Deserialize: string -> Result<'T,string>)> str =
        (^T : (static member Deserialize: string -> Result<'T,string>) str)

    /// ## Example
    /// ```fsharp
    /// type Item =
    ///     {
    ///         Name: string
    ///         Cost: int
    ///     }
    ///
    ///     static member Empty : Item =
    ///         {
    ///             Name = ""
    ///             Cost = 0
    ///         }
    /// (empty : Item) // { Name = ""; Cost = 0 }
    /// ```
    let inline empty<'T when 'T : (static member Empty: 'T)> =
        (^T : (static member Empty: 'T) ())
