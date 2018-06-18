[<RequireQualifiedAccess>]
module FsharpMyExtension.Map
//module Map =
open FsharpMyExtension.FSharpExt

(*let choiceFold fn m = Seq.fold (fun st (k:'key, v:'value) -> fn k v |> function Some x -> Map.add k x st | None -> Map.remove k st ) m (Map.toSeq m) *)
let choose fn (m:Map<_,_>) =
    m |> Seq.choose (function KeyValue(k:'key, v:'value) -> fn k v |> Option.map (fun x -> k, x))
    |> Map.ofSeq

let values (m:Map<_,_>) = m |> Seq.map (function KeyValue(k:'key, v:'value) -> v )
let keys (m:Map<_,_>) = m |> Seq.map (function KeyValue(k:'key, v:'value) -> k )
let chooseS fn (m:Map<_,_>) =
    m |> Seq.choose (function KeyValue(k:'key, v:'value) -> fn k v)
let filterS (fn:'key -> 'value -> _) (m:Map<_,_>) =
    // m |> Seq.filter (function KeyValue(k:'key, v:'value) -> fn k v)
    // |> Seq.map (fun x -> x)
    chooseS (fun x y -> if fn x y then Some y else None) m
    //Map.filter fn >> values
    //: Map<_,_> -> _
    //choose (fun k v -> if fn k v then Some v else None)
/// жаль что нет встроенной реализации. А так, приходится дважды искать ключ, хотя можно было бы сначала найти (если есть) и заменить значение.
let addOrMod key def modify db =
    Map.tryFind key db
    |> Option.map modify
    |> Option.defaultValue def
    |> flip (Map.add key) db

let addOrModWith key def modify db =
    Map.tryFind key db
    |> Option.map modify
    |> Option.defaultWith def
    |> flip (Map.add key) db


(*
let chooseFilMap fn m = 
    let acc = ref []
    m |> Map.filter (fun k x -> fn k x |> function Some x -> acc := x :: !acc; true | None -> false)
    |> fun xs -> acc := List.rev !acc; xs |> Map.map (fun _ _ -> List.head !acc |> fun x -> acc := List.tail !acc; x )

assert
    let m = [1..600000] |> List.map (fun x -> string x, x) |> Map.ofList
    let fn = (fun _ v -> if v % 2 = 0 then Some (sprintf "digit - %d" v) else None)
    let s = chooseSeq fn m
    let fil = chooseFilMap fn m
    s = fil *)
    
/// reducef id (fun k v -> k::v) [] Map.empty [1;2;2;3;4;5;-1] = map [(-1, [-1]); (1, [1]); (2, [2; 2]); (3, [3]); (4, [4]); (5, [5])]
let reducef key fn def =
    Seq.fold (fun st x ->
        // x |> (key
        //     >> s (fun k v -> Map.add k (fn x v) st)
        //         (flip Map.tryFind st >> flip defaultArg def)))
        key x
        |> s (fun k v -> Map.add k (fn x v) st)
            (flip Map.tryFind st >> flip defaultArg def))

assert
    let ran = System.Random() |> fun x -> fun min max -> x.Next(min, max)
    let lst = List.init 10 (fun _ -> ran 0 10)
    let actual = lst |> reducef id (fun k v -> k::v) [] Map.empty
    let expect = lst |> Seq.groupBy id |> Seq.map (mapSnd List.ofSeq) |> Map.ofSeq
    actual = expect


/// reducef id (fun k v -> k::v) [] Map.empty [1;2;2;3;4;5;-1] = map [(-1, [-1]); (1, [1]); (2, [2; 2]); (3, [3]); (4, [4]); (5, [5])]
let reducef2 key fn def x st =
        // x |> (key
        //     >> s (fun k v -> Map.add k (fn x v) st)
        //         (flip Map.tryFind st >> flip defaultArg def)))
        key x
        |> s (fun k v -> Map.add k (fn x v) st)
            (flip Map.tryFind st >> flip defaultArg def)