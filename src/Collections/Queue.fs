namespace FsharpMyExtension.Collections

[<Struct>]
type 'a Queue = Q of 'a list * 'a list
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Queue =
    let rec deq = function
        | Q((x::xs), ys) -> x, Q(xs, ys)
        | Q([], []) -> failwith "queue empty"
        | Q([], ys) -> deq (Q((List.rev ys), []))
    let enq (Q(xs, ys)) y = Q(xs, y::ys)
    let empty = Q([], [])
    let isEmpty = function Q([], []) -> true | _ -> false
    //let q = enq (enq <| enq empty 1 <| 2) 3
    //deq q |> snd |> flip enq 4 |> deq
    //deq (Queue (x:xs) ys) = (x, Queue xs ys)
    //System.Collections.Generic.Queue
