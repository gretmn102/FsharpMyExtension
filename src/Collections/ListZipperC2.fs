namespace FsharpMyExtension.Collections.ListZipperCircle2
open FsharpMyExtension.Collections

type Place = Middle | EndR | EndL

type 'a LZC = {
    State: Place * ListZ<'a>
}
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module LZC =
    open FsharpMyExtension

    let toList (lz:_ LZC) = lz.State |> snd |> ListZ.toList
    let isEndR' lz =
        List.isEmpty lz.Left
    let isEndL' lz =
        List.isEmpty lz.Right

    let lzc x = { State = x }

    let ofList xs =
        { State = Middle, ListZ.ofList xs }
    let bind (f: _ -> _ LZC) lst = lst.State |> f

    let isSingletone (lzc:_ LZC) =
        lzc.State |> snd
        |> ListZ.isSingleton

    let map f lst = lst.State |> f |> lzc
    let update' f = map (snd >> f)

    let endR x = x |> cond isEndR' (comma EndR) (comma Middle)
    let endL x = x |> cond isEndL' (comma EndL) (comma Middle)

    let next lz =
        let toStart x = ListZ.toStart x |> endR
        lz |> update' (fun xs ->
            ListZ.next xs
            |> Option.map endR
            |> Option.defaultWith (fun () -> toStart xs))

    let prev lz =
        let toEnd = ListZ.toEnd >> endL
        lz |> update' (fun xs ->
            ListZ.prev xs
            |> Option.map endL
            |> Option.defaultWith (fun () -> toEnd xs))

    let removeR lz =
        let _, lz = lz.State

        ListZ.removeR lz
        |> Option.map (fun x ->
            if List.isEmpty lz.Left then
                lzc (Middle, x) |> next
            else
                lzc (endR x)
        )
    let removeL lz =
        let _, lz = lz.State

        ListZ.removeL lz
        |> Option.map (fun x ->
            if List.isEmpty lz.Right then
                lzc (Middle, x) |> prev
            else
                lzc (endL x) )

    let nexts i lzc =
        (i, lzc)
        |> until (fst >> (>) 1)
            (fun (i, lzc) -> i - 1, next lzc)
        |> snd
    let hole lst = lst.State |> (snd >> ListZ.hole)
    let pos lst = lst.State |> fst
    let update'' f = map (mapSnd f)
    let update f = update'' (ListZ.update f)
    let set x = update (k x)
    let insertAfter x = update'' (ListZ.insertAfter x)
    let insertBefore x = update'' (ListZ.insertBefore x)


