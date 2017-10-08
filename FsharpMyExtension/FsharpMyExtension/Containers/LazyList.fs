module LazyList

type LazyList<'a> = 
   | Empty 
   | Cons of 'a * Lazy<LazyList<'a>> 
let head = function
    | Cons (h, _) -> h 
    | Empty -> failwith "empty list" 
let tail = function
    | Cons (_, t) -> t.Force() 
    | Empty -> failwith "empty list"

let single a = Cons (a, lazy ( Empty ))

let cons (a : 'a) (l : LazyList<'a>) = Cons (a, lazy ( l )) 

let rec map f = function
    | Empty -> Empty
    | Cons (a, t) -> Cons (f a, lazy (map f (t.Force())))

let rec iter f = function
    | Empty -> () 
    | Cons (a, t) -> f a; iter f (t.Force())

let rec take nr = function
    | Empty -> Empty 
    | Cons (a, t) -> 
        if nr = 0 then Empty 
        else Cons (a, lazy (take (nr-1) (t.Force())))

let rec unfold (f : 's -> ('a*'s) option) (init : 's) : LazyList<'a> = 
    match f init with 
    | None -> Empty 
    | Some (a, s) -> Cons (a, lazy ( unfold f s))

let rec foldr (f : 'a -> Lazy<'s> -> 's) (init : 's) = function
   | Cons (a, t) -> f a (lazy (foldr f init (t.Force())))
   | Empty -> init
//
//type 'a T =
//    | T of (unit -> 'a T)
//    | Elem of 'a
//
//let rec next = function
//    | T f -> f() |> next
//    | Elem x -> x
//
//let fold fn =
//    let rec f acc = function
//        | h::t -> fn acc h |> fun acc -> T(fun () -> f acc t)
//        | [] -> Elem acc
//    f
//fold (fun st x -> x::st) [] [1..10] |> next

//assert
//    List.fold (fun st x -> x::st) [] [1..10] = fold (fun st x -> x::st) [] [1..10]
(*let ff fn ini ini' xs = Seq.fold fn ini xs ini'
let fn = fun f x st -> if st = 0 then [] else x :: f (st - 1)
ff (fun f -> printfn "eval"; fn f ) (fun _ -> []) 3 ['a'..'z'] *)