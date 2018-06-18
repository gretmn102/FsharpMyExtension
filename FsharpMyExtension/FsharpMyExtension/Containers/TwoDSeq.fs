module FsharpMyExtension.TwoDSeq

module TwoDOp =
    type T<'a,'b> =
        {
            Get: int -> int -> 'a
            Set: int -> int -> 'b -> unit
            Length1: int
            Length2: int
        }
    let iteri f x =
        for i = 0 to x.Length1 - 1 do
            for j = 0 to x.Length2 - 1 do
                f i j <| x.Get i j
    let map f t = iteri (fun i j x -> t.Set i j (f x)) t

    let iterFoldi f st t =
        let mutable st = st
        for i = 0 to t.Length1 - 1 do
            for j = 0 to t.Length2 - 1 do
                let st' = f st (i, j) (t.Get i j)
                st <- st'
        st
    let mapFoldi f st t =
        iterFoldi (fun st (i,j) x ->
                    let c, st = f st (i,j) x
                    t.Set i j c
                    st ) st t
    let iterPar f x =
        let xs = [| 0..x.Length2 - 1|]
        for i = 0 to x.Length1 - 1 do
            xs |> Array.Parallel.iter (fun j -> f i j (x.Get i j))
    let mapP f t = iterPar (fun i j x -> t.Set i j (f x)) t
    let mapiP f t = iterPar (fun i j x -> t.Set i j (f i j x)) t