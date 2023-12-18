namespace FsharpMyExtension.Containers.Either

[<Struct>]
type Either<'a,'b> =
    | Left of Left : 'a
    | Right of Right : 'b
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Either =
    let empty = Right

    let map fn = function
        | Right x -> Right(fn x)
        | Left x -> Left x
    /// `either (f >> Left) (g >> Right)`
    let mapBoth f g = function
        | Right x -> Right(g x)
        | Left x -> Left(f x)
    let mapLeft f = function Left x -> Left(f x) | Right x -> Right x
    let iter fn = function
        | Right x -> fn x
        | Left _ -> ()
    let fold fn state = function
        | Right x -> fn state x
        | _ -> state
    let bind fn = function
        | Right x -> fn x
        | Left x -> Left x
    let liftA2 fn x y =
        match x, y with
        | Right x, Right y -> fn x y |> Right
        | Left x, _ -> Left x
        | _, Left x -> Left x
    let ap (f : Either<'a, ('b -> 'c)>) xs =
        match f, xs with
        | Right x, Right y -> Right(x y)
        | Left x, _ -> Left x
        | _, Left x -> Left x
    /// Case analysis for the Either type. If the value is Left a, apply the first function to a; if it is Right b, apply the second function to b.
    let either f g = function
        | Right x -> g x
        | Left x -> f x

    let ofOption s = function None -> Left s | Some x -> Right x
    let ofOptionWith s = function None -> Left(s()) | Some x -> Right x

    /// непохоже на sequenceA, ибо `f<t, f<x>> -> t<f<x>>` и результат не равен `fmap concat . sequenceA`
    let seqEitherPseudo xs = either (Left >> Seq.singleton) id xs

    let listEitherPseudo xs = either (Left >> List.singleton) id xs
    let collect f = map f >> seqEitherPseudo

    let getOrDef x = function Right x -> x | _ -> x
    let getOrElse f = function
        | Right x -> x
        | Left _ -> f()
    let getOrDef' fn = function Right x -> x | Left x -> fn x
    let orElse x = function
        | Right x -> Right x
        | Left _ -> x
    let orElseWith f = function
        | Right x -> Right x
        | Left _ -> f()
    let get = function Right x -> x | x -> failwithf "try get right, but '%A'" x
    let getLeft = function Left x -> x | x -> failwithf "try get left, but '%A'" x
    let isRight = function Right _ -> true | _ -> false
    let isLeft = function Left _ -> true | _ -> false

    let concat x = bind id x

    let travOpt (rf: 'a -> option<'b>) (x : Either<'c,'a>) : option<Either<'c,'b>> =
        match x with
        | Left x -> Some (Left x)
        | Right x ->  (rf >> Option.map Right) x
    let seqOpt x = travOpt id x

    let toResult = function
        | Right x -> Ok x
        | Left x -> Error x
    let ofResult = function
        | Ok x -> Right x
        | Error x -> Left x

module EitherOperators =
    let (<*>) x f = Either.ap f x
    let (>>=) x f = Either.bind f x
    let (>=>) f g x = Either.bind g (f x)
    let preturn x = Right x
    let (>>.) p1 p2 = p1 >>= fun _ -> p2
    let (>>%) p x = p >>= fun _ -> (preturn x)
    let (.>>) p1 p2 = p1 >>= (>>%) p2
    let (|>>) p f = p >>= (f >> preturn)
    let (.>>.) p1 p2 =
        p1 >>= fun x -> p2 |>> fun y -> x, y
    let (<|>) p1 p2 = Either.orElse p1 p2

[<AutoOpen>]
module EitherExtensions =
    [<RequireQualifiedAccess>]
    module List =
        let travEither (fn: 'a -> Either<'b,'c>) (xs:'a list) : Either<'b, 'c list> =
            let rec f acc = function
                | x::xs ->
                    match fn x with
                    | Right x -> f (x::acc) xs
                    | Left x -> Left x
                | [] -> Right (List.rev acc)
            f [] xs

        let seqEither xs =
            travEither id xs

        let partitionEithers xs =
            xs
            |> List.partition (Either.isLeft)
            |> fun (xs, ys) -> List.map Either.getLeft xs, List.map Either.get ys

    [<RequireQualifiedAccess>]
    module Seq =
        let travEither fn (xs : _ seq) =
            let rec f acc (xs:System.Collections.Generic.IEnumerator<_>) =
                if xs.MoveNext() then
                    xs.Current |> fn |> function
                        | Right x -> f (x :: acc) xs
                        | Left x -> Left x
                else List.rev acc |> Right
            f [] <| xs.GetEnumerator()

        let seqEither xs = travEither id xs
        // open FsharpMyExtension.FSharpExt
        // let partitionEithers (xs : _ seq) =
        //     let xs = xs.GetEnumerator()
        //     let rec f acc =
        //         if xs.MoveNext() then
        //             xs.Current |> function
        //                 | Right x -> f (mapSnd (List.cons x) acc)
        //                 | Left x  -> f (mapFst (List.cons x) acc)
        //         else mapPair List.rev List.rev acc
        //     f ([],[])

    [<RequireQualifiedAccess>]
    module Option =
        let ofEither x = x |> Either.either (fun _ -> None) Some
        let travEither (f : 'a -> Either<'c, 'b>) (x:Option<'a>) =
            match x with
            | Some x ->
                // f x |> Either.map Some
                match f x with
                | Right x -> Right(Some x)
                | Left x -> Left x
            | None -> Right None
        let seqEither x = travEither id x

    [<RequireQualifiedAccess>]
    module Result =
        let ofEither = function
            | Right x -> Ok x
            | Left x -> Error x
        let toEither = function
            | Ok x -> Right x
            | Error x -> Left x
