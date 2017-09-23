namespace FsharpMyExtension.Either
//#if INTERACTIVE
//#load "library1.fs"
//#load "list.fs"
//#endif
open FsharpMyExtension.List

type Either<'a,'b> = Left of 'a | Right of 'b
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Either =
    let map fn = function
        | Right x -> Right <| fn x
        | Left x -> Left x
    let fold fn state = function
        | Right x -> fn state x
        | _ -> state
    let bind fn = function
        | Right x -> fn x
        | Left x -> Left x
    /// Case analysis for the Either type. If the value is Left a, apply the first function to a; if it is Right b, apply the second function to b.
    let either f g = function
        | Right x -> g x
        | Left x -> f x
    /// either (f >> Left) (g >> Right)
    let eitherE f g = either (f >> Left) (g >> Right)

    let ofOption s = function None -> Left s | Some x -> Right x
    
    //let travEitherPseudo f xs = either (Left >> Seq.singleton) (Seq.map <| bind f) xs
    /// не похоже на sequenceA ибо f (t (f x)) -> t (f x). И результат не равен: "fmap concat . sequenceA"
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

    let isRight = function Right _ -> true | _ -> false
    let isLeft = function Left _ -> true | _ -> false
    let concat x = bind id x

[<RequireQualifiedAccess>]
module List =
    [<System.ObsoleteAttribute("Renaming in seqEither", true)>]
    let travEither xs =
        xs |> List.takeWhile Either.isRight
        |> function
            | xs, [] ->
                List.map (Either.getOrDef' (fun _ -> failwith "" )) xs |> Right | _, Left x::_ -> Left x
            | _ -> failwith ""
    
    let seqEither xs =
        xs |> List.takeWhile Either.isRight
        |> function
            | xs, [] ->
                List.map (Either.getOrDef' (fun _ -> failwith "" )) xs |> Right | _, Left x::_ -> Left x
            | _ -> failwith ""        

[<RequireQualifiedAccess>]
module Seq =
    let travEither fn (xs : _ seq) =
        let rec f acc (xs:System.Collections.Generic.IEnumerator<_>) = 
            if xs.MoveNext() then
                xs.Current |> fn |> Either.bind (fun x -> f (x :: acc) xs)
            else List.rev acc |> Right
        f [] <| xs.GetEnumerator()

    let seqEither xs = travEither id xs

[<RequireQualifiedAccess>]
module Option =
    let ofEither x = x |> Either.either (fun _ -> None) Some