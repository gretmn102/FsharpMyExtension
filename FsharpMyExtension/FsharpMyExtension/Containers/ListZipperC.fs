namespace FsharpMyExtension.ListZipperCircle
type Place = Middle | EndR | EndL
type 'a Elem = Empty | Elem of Place * 'a
type 'a ListZC = Lz of 'a list * 'a Elem * 'a list
module ListZC = 
    let ofList = function
        | h::t -> Lz(t, Elem(Middle, h), [])
        | [] -> Lz([], Empty, [])

    let next = function
        | Lz(_, Empty, _) as x -> x
        | Lz([], Elem(_, x), []) -> Lz([], Elem(EndR, x), [])
        | Lz([], Elem(_, x), r) -> 
            match List.rev (x::r) with
            | h::t -> Lz(t, Elem(Middle, h), [])
            | [] -> failwith "List.rev (x::r) = empty"
        | Lz([lx], Elem(_, x), r) -> 
            Lz([], Elem(EndR, lx), x::r)
        | Lz(lx::l, Elem(_, x), r) ->
            Lz(l, Elem(Middle, lx), x::r)
    let prev = function
        | Lz(_, Empty, _) as x -> x
        | Lz([], Elem(_, x), []) -> Lz([], Elem(EndL, x), [])
        | Lz(l, Elem(_, x), []) -> 
            match List.rev (x::l) with
            | h::t -> Lz([], Elem(Middle, h), t)
            | [] -> failwith "List.rev (x::r) = empty"
        | Lz(l, Elem(_, x), [rx]) -> 
            Lz(x::l, Elem(EndL, rx), [])
        | Lz(l, Elem(_, x), rx::r) ->
            Lz(x::l, Elem(Middle, rx), r)
    let rec removeR = function
        | Lz(_, Empty, _) as x -> x
        | Lz([], _, []) -> Lz([], Empty, [])
        | Lz([], _, r) ->
            match List.rev r with
            | h::t -> Lz(t, Elem(Middle, h), [])
            | [] -> failwith "List.rev (x::r) = empty"
        | Lz([lx], _, r) -> 
            Lz([], Elem(EndR, lx), r)
        | Lz(lx::l, _, r) ->
            Lz(l, Elem(Middle, lx), r)
    let update fn = function
        | Lz(_, Empty, _) as x -> x
        | Lz(l, Elem(pos, x), r) -> Lz(l, Elem(pos, fn x), r)
    let isEmpty = function
        | Lz(_, Empty, _) -> true
        | _ -> false
    let getElem = function
        | Lz(_, Elem(_, x), _) -> x
        | _ -> failwith "element empty"
    let toList = function
        | Lz(_, Empty, _) -> []
        | Lz(l, Elem(_, x), r) -> List.append <| List.rev r <| x::l
    let map fn = function
        | Lz(_, Empty, _) -> Lz([], Empty, [])
        | Lz(l, Elem(pos, x), r) -> Lz(List.map fn l, Elem(pos, fn x), List.map fn r)
    let rec choose fn = function
        | Lz(_, Empty, _) -> Lz([], Empty, [])
        | Lz(l, Elem(pos, x), r) as curr-> 
            match fn x with
            | Some x -> Lz(List.choose fn l, Elem(pos, x), List.choose fn r)
            | None -> choose fn <| removeR curr
                (*match List.choose fn l with
                | [] -> failwith ""
                | h::t -> LZ(t, Elem(pos, h), List.choose fn r) *)
    assert
        let xs = ofList [1..3]

        let get = function
            | Lz(_, Elem(_, x), _) -> x
            | _ -> failwith "get empty exception"
        let f n fn = 
            xs |> Seq.unfold (function Lz(_, Empty, _) -> None | st -> let x = fn st in Some(get st, x))
            |> Seq.take n |> List.ofSeq
        assert
            f 10 prev = [1; 3; 2; 1; 3; 2; 1; 3; 2; 1]
        assert
            f 10 next = [1; 2; 3; 1; 2; 3; 1; 2; 3; 1]
        assert
            f 3 removeR = [1..3]
        assert
            let xs = [1..10]
            ofList xs |> next |> next |> next |> toList = xs
        assert
            let xs = ofList [1..10]
            let f x = if x % 2 = 0 then Some x else None
            assert
                next xs |> choose f = ofList[2..2..10]
            assert
                next xs |> next |> choose f = (ofList[2..2..10] |> next)
            false
        xs |> removeR |> next |> removeR = Lz([],Elem (Middle,2),[])