namespace FsharpMyExtension.Option

[<RequireQualifiedAccess>]
module Option =
    let getOrDef f = function Some x -> x | _ -> f()
    let ofNull = function null -> None | x -> Some x
    let concat x = Option.bind id x

[<RequireQualifiedAccess>]
module Seq =
    let travOpt fn (xs : _ seq) =
        let rec f acc (xs:System.Collections.Generic.IEnumerator<_>) = 
            if xs.MoveNext() then
                xs.Current |> fn |> Option.bind (fun x -> f (x :: acc) xs)
            else List.rev acc |> Some
        f [] <| xs.GetEnumerator()

    let seqOpt xs = travOpt id xs
    assert
        Seq.init 10 Some |> seqOpt = Some [0..9]
    assert
        seq{ yield Some 1; yield None; yield Some 3 } |> seqOpt = None
    assert // lazyness test
        let counter = ref 0
        seq{ yield Some 1; incr counter; yield None; incr counter; yield Some 3; incr counter; } |> seqOpt |> ignore
        !counter = 1