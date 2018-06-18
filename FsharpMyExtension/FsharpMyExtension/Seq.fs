[<RequireQualifiedAccess>]
module FsharpMyExtension.Seq
let rec transpose xss = 
    if Seq.isEmpty xss || Seq.isEmpty (Seq.head xss) then
        Seq.empty
    else
        seq{
            yield Seq.map Seq.head xss
            yield! transpose (Seq.map Seq.tail xss) 
        }
let headTail (xs: _ seq) =
    let xs = xs.GetEnumerator()
    xs.MoveNext() |> ignore
    xs.Current, seq{ while xs.MoveNext() do yield xs.Current }

let rec foldr f st (xs: _ seq) =
    let xs = xs.GetEnumerator()
    let rec foldr f st =
        if xs.MoveNext() then
            f xs.Current (lazy (foldr f st))
        else st
    foldr f st


// module T =
//     (*type List<'a> =
//         | Cons of 'a * List<'a>
//         | Nil
//     let (>!) x xs = Cons(x, xs) *)
//     let rec foldBack f st = function
//         | x :: xs -> foldBack f (f x st) xs
//         | [] -> st
    
    
//     //((a + b) + c)
//     List.rev [1..10] |> foldBack (fun x st -> x::st) []
//     let rec fold f st = function
//         | x::xs -> f x (lazy(fold f st xs))
//         | [] -> st
//     let rec fold' fn st =
//         let rec f st = function
//             | x::xs -> fn x  (lazy(f st xs))
//             | [] -> st
//         f st
//     let i = ref 0
//     let fn step ini (xs:'T list) n = fold step ini xs n
//     let take n xs = fn (fun x st -> function 0 -> [] | n -> x :: (st.Value (n - 1))) (fun _ -> []) xs n
//     take 20000 [1..20000]
//     Seq.unfold (fun (xs, ys) -> ys |> function h::t -> Some((h, t), t) | [] -> None ) ([], [1..10])


let travOpt fn (xs : _ seq) =
    let rec f acc (xs:System.Collections.Generic.IEnumerator<_>) = 
        if xs.MoveNext() then
            xs.Current |> fn |> //Option.bind (fun x -> f (x :: acc) xs)
                function None -> None | Some x -> f (x::acc) xs
        else List.rev acc |> Some
    f [] <| xs.GetEnumerator()

let seqOpt xs = travOpt id xs