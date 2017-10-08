namespace FsharpMyExtension.Option

[<RequireQualifiedAccess>]
module Option =
    [<System.ObsoleteAttribute("`Option.defaultWith` in FSharp v4")>]    
    let getOrDef f = function Some x -> x | _ -> f()
    [<System.ObsoleteAttribute("`Option.ofObject` in FSharp v4")>]
    let ofNull = function null -> None | x -> Some x
    [<System.ObsoleteAttribute("`Option.flatten` in FSharp v4")>]
    let concat x = Option.bind id x
[<RequireQualifiedAccess>]
module Seq =
    let travOpt fn (xs : _ seq) =
        let rec f acc (xs:System.Collections.Generic.IEnumerator<_>) = 
            if xs.MoveNext() then
                xs.Current |> fn |> //Option.bind (fun x -> f (x :: acc) xs)
                    function None -> None | Some x -> f (x::acc) xs
            else List.rev acc |> Some
        f [] <| xs.GetEnumerator()

    let seqOpt xs = travOpt id xs
