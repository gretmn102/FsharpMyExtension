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
    module Parallel =
        let iter f x =
            // Array.Parallel.init x.Length1 (fun i ->
            //     Array.Parallel.init x.Length2 (fun j -> f i j (x.Get i j))
            //     |> ignore
            // ) |> ignore
            System.Threading.Tasks.Parallel.For(0, x.Length1,
                fun i ->
                    System.Threading.Tasks.Parallel.For(0, x.Length2, fun j ->
                        f i j (x.Get i j)
                    ) |> ignore
            ) |> ignore
        let map f t = iter (fun i j x -> t.Set i j (f x)) t
        let mapi f t = iter (fun i j x -> t.Set i j (f i j x)) t