namespace FsharpMyExtension.ListZipper
open FsharpMyExtension.FSharpExt
open FsharpMyExtension.ListZipper
type 'a LZLazy = {
    LZ: 'a ListZ
    // St: System.Collections.Generic.IEnumerator<'a>
    St: 'a seq
}
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module LZLazy =
    let private hd (xs: _ seq) =
        Seq.tryHead xs
        |> Option.map (fun x -> x, Seq.tail xs)
    let ofSeq (xs:_ seq) =
        hd xs
        |> Option.map (fun (x,xs) ->
            {
                LZ = ListZ.singleton x
                St = xs
            }
        )
    let next (st:'a LZLazy) =
        ListZ.next st.LZ
        |> Option.map (fun lz ->
            Some { st with LZ = lz }
        )
        |> Option.defaultWith (fun () ->
            hd st.St
            |> Option.map (fun (x,xs) ->
                { LZ = ListZ.insertBefore x st.LZ; St = xs }
            )
        )
    let prev (st:'a LZLazy) =
        ListZ.prev st.LZ
        |> Option.map (fun lz -> { st with LZ = lz })
    let hole (st:'a LZLazy) = st.LZ |> ListZ.hole
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