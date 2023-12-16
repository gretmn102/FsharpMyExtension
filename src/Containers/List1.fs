namespace FsharpMyExtension

type List1<'a> = 'a * 'a list
module List1 =
    open FsharpMyExtension
    let head : List1<'a> -> 'a = fst
    let tail = let fn xs = snd xs |> on List.head List.tail in fn : (List1<'a> -> List1<'a>)
    let (|Many|One|) =
        let fn x =
            match x with
            | x, [] -> One x
            | x, xs -> Many((x, xs) : List1<'a> )
        in fn : (List1<_> -> Choice<List1<_>, _>)
    let fn =
        let f = function
        | Many(x, xs) -> "many"
        | One x -> "only one"
        in f : (List1<_> -> _)
    let create = comma : (_ -> _ -> List1<_>)
