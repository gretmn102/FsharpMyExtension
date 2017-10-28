namespace FsharpMyExtension.Either
open FsharpMyExtension.List

type Either<'a,'b> = Left of 'a | Right of 'b
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Either =
    let empty = Right

    let map fn = function
        | Right x -> Right <| fn x
        | Left x -> Left x
    /// either (f >> Left) (g >> Right)
    let mapBoth f g = function
        | Right x -> Right(g x)
        | Left x -> Left(f x)
    let mapLeft f = function Left x -> Left(f x) | Right x -> Right x

    let fold fn state = function
        | Right x -> fn state x
        | _ -> state
    let bind fn = function
        | Right x -> fn x
        | Left x -> Left x
    let liftA2 fn x y = 
        //x |> bind (fun x -> map (fn x) y)
        match x, y with
        | Right x, Right y -> fn x y |> Right
        | Left x, _ -> Left x
        | _, Left x -> Left x        
    let (<*>) (f : Either<'a, ('b -> 'c)>) xs =
        //bind (fun x -> map x xs) f
        match f, xs with
        | Right x, Right y -> Right <| x y
        | Left x, _ -> Left x
        | _, Left x -> Left x
    /// Case analysis for the Either type. If the value is Left a, apply the first function to a; if it is Right b, apply the second function to b.
    let either f g = function
        | Right x -> g x
        | Left x -> f x
    [<System.ObsoleteAttribute("use `mapBoth`", true)>]
    let eitherE f g = either (f >> Left) (g >> Right)

    let ofOption s = function None -> Left s | Some x -> Right x
    let ofOptionWith s = function None -> Left(s()) | Some x -> Right x
    
    //let travEitherPseudo f xs = either (Left >> Seq.singleton) (Seq.map <| bind f) xs
    /// не похоже на sequenceA ибо f<t, f<x>> -> t<f<x>>. И результат не равен: "fmap concat . sequenceA"
    let seqEitherPseudo xs = either (Left >> Seq.singleton) id xs
    assert
        [
        Left "0" |> seqEitherPseudo |> List.ofSeq = [Left "0"]
        Right (seq[ Left "0"; Right 1 ]) |> seqEitherPseudo |> List.ofSeq = [Left "0"; Right 1]
        Right (seq[ ]) |> seqEitherPseudo |> List.ofSeq |> List.isEmpty ] |> List.forall id

    let collect f = map f >> seqEitherPseudo
    assert
        [
            Right 0 |> collect (Seq.singleton << Right) |> List.ofSeq = [Right 0]
            Left "error" |> collect (Seq.singleton << Right) |> List.ofSeq = [Left "error"]
            Right () |> collect (fun _ -> seq [ Right 0; Left "left"; Right 1 ]) |> List.ofSeq = [Right 0; Left "left"; Right 1]
        ] |> List.forall id
    
    let getOrDef x = function Right x -> x | _ -> x
    let getOrDef' fn = function Right x -> x | Left x -> fn x
    let get = function Right x -> x | x -> failwithf "try get right, but '%A'" x
    let getLeft = function Left x -> x | x -> failwithf "try get left, but '%A'" x
    let isRight = function Right _ -> true | _ -> false
    let isLeft = function Left _ -> true | _ -> false
    let concat x = bind id x

[<RequireQualifiedAccess>]
module List =
    let travEither (fn: 'a -> Either<'b,'c>) (xs:'a list) : Either<'b, 'c list> =
        let rec f acc = function
            | x::xs ->
                fn x // |> Either.bind (fun x -> f (x::acc) xs) // хуже работает
                |> function
                    | Right x -> f (x::acc) xs
                    | Left x -> Left x
            | [] -> Right acc
        f [] xs

    let seqEither xs =
        // travEither id xs // дольше?
        xs |> List.takeWhileRest Either.isRight
        |> function
            | xs, [] -> List.map Either.get xs |> Right
            | _, Left x::_ -> Left x
            | _ -> failwith ""
    open FsharpMyExtension.FSharpExt
    let partitionEithers xs =
        xs |> List.partition (Either.isLeft)
        |> mapPair (List.map Either.getLeft) (List.map Either.get)

    // assert
    //     let xs : list<Either<unit, _>> = List.init 500000 Right
    //     travEither id xs |> ignore
    //     seqEither xs |> ignore
    //     true
    // let seqEither f = List.map f >> seqEither
    // let ss xs = s id xs
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

[<RequireQualifiedAccess>]
module Option =
    let ofEither x = x |> Either.either (fun _ -> None) Some