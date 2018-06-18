namespace FsharpMyExtension.ListZipper
open FsharpMyExtension.FSharpExt
open FsharpMyExtension.ListZipper
type LZLazy2<'a, 'b> = {
    LZ: 'b ListZ
    // St: System.Collections.Generic.IEnumerator<'a>
    RightLazy: 'a seq
    LeftLazy: 'a seq
    Map:('a -> 'b)
}
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module LZLazy2 =
    let private hd (xs: _ seq) =
        Seq.tryHead xs
        |> Option.map (fun x -> x, Seq.tail xs)
    let ofSeq (f:'a -> 'b) (xs:_ seq) =
        hd xs
        |> Option.map (fun (x,xs) ->
            {
                Map = f
                LZ = ListZ.singleton (f x)
                RightLazy = xs
                LeftLazy = Seq.empty
            }
        )
    let init f (left: _ seq) x (right:_ seq) =
        // hd right
        // |> Option.map (fun (x,xs) ->
        {
            Map = f
            LZ = ListZ.singleton x
            RightLazy = right
            LeftLazy = left
        }
        // )
    let next (st: LZLazy2<_,_>) =
        ListZ.next st.LZ
        |> Option.map (fun lz -> Some { st with LZ = lz })
        |> Option.defaultWith (fun () ->
            hd st.RightLazy
            |> Option.map (fun (x,xs) ->
                { st with LZ = ListZ.insertBefore (st.Map x) st.LZ; RightLazy = xs }
            )
        )
    let prev (st:LZLazy2<_,_>) =
        // ListZ.prev st.LZ
        // |> Option.map (fun lz -> { st with LZ = lz })
        ListZ.prev st.LZ
        |> Option.map (fun lz -> Some { st with LZ = lz })
        |> Option.defaultWith (fun () ->
            hd st.LeftLazy
            |> Option.map (fun (x,xs) ->
                { st with LZ = ListZ.insertAfter (st.Map x) st.LZ; LeftLazy = xs }
            )
        )
    let hole (st: LZLazy2<_,_>) = st.LZ |> ListZ.hole
    
    module Prototype =
        type SerializeTyp<'Id> = 'Id ListZ
        type WorkedTyp<'Id,'Data> = {
            LZ: 'Data ListZ
            Left:'Id list
            Right:'Id list
            GetData : 'Id -> 'Data
        }
        let private hd = function
            | x::xs -> Some(x, xs)
            | [] -> None

        let ofSerTyp (getData: 'Id -> 'Data) (st: _ SerializeTyp) = 
            {
                LZ = ListZ.singleton (getData st.Current)
                Left = st.Left
                Right = st.Right
                GetData = getData
            }
        // let toSerTyp (getId: 'Data -> 'Id) (writeDataToDb: 'Data -> unit) (st: WorkedTyp<'Id,'Data>) =
        let toSerTyp (getId: 'Data -> 'Id) writeDataToDb (st: WorkedTyp<_,_>) =
            // st.LZ |> ListZ.iter writeDataToDb
            let f x = writeDataToDb x; getId x
            let lz = st.LZ
            let x = ListZ.singleton (f lz.Current)
            {
                x with
                    ListZ.Left = st.Left @ List.map f lz.Left
                    ListZ.Right = st.Right @ List.map f lz.Right
            } : SerializeTyp<_>
        // let next2 : WorkedType2 -> WorkedType2 =
        //     ()
        //     fun (((w,ser): WorkedType2)) ->
        //         ListZ.next w
        //         |> Option.map (fun lz -> Some (lz, ser))
        //         |> Option.defaultWith (fun () ->
        //             hd w.Right
        //             |> Option.map (fun (x,xs) ->
        //                 ListZ.next ser
        //             )
        //             // failwith ""
        //         )
        let next (st:WorkedTyp<'Id,'Data>) =
            ListZ.next st.LZ
            |> Option.map (fun lz -> Some { st with LZ = lz })
            |> Option.defaultWith (fun () ->
                hd st.Right
                |> Option.map (fun (x,xs) ->
                    { st with LZ = ListZ.insertBefore (st.GetData x) st.LZ; Right = xs }
                )
            )
        let prev (st:WorkedTyp<'Id,'Data>) =
            ListZ.prev st.LZ
            |> Option.map (fun lz -> Some { st with LZ = lz })
            |> Option.defaultWith (fun () ->
                hd st.Left
                |> Option.map (fun (x,xs) ->
                    { st with LZ = ListZ.insertAfter (st.GetData x) st.LZ; Left = xs }
                )
            )

        

        
// let xs = Seq.append (seq{1..20}) (seq{ yield failwith "fail"})
// let x = start xs |> Option.get
// next x |> Option.bind next
// let f () =
//     let (x, xs') = hd xs |> Option.get
//     let (x, xs'') = hd xs' |> Option.get
//     xs, xs', xs''
//     xs
// x.St
// hole x
// next x |> Option.map hole
// x
// xs |> Seq.head
    // |> Option.map (fun lz ->
    //     )
// let rec next = function
//     | Next(_,_,p) -> p() |> next
// let en:int = start xs |> next
// match start xs with
// | Next(n, c, p) ->
//     match p() with
//     | Next(n, c, p) -> p()