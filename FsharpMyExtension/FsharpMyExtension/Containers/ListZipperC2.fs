namespace FsharpMyExtension.ListZipperCircle2
open FsharpMyExtension.FSharpExt

type Place = Middle | EndR | EndL
type 'a LZC = {
    State:Place * FsharpMyExtension.ListZipper.ListZ<'a>
}
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module LZC =
    open FsharpMyExtension
    open FsharpMyExtension.ListZipper
    let toList (lz:_ LZC) = lz.State |> snd |> ListZ.toList
    let isEndR' lz =
        List.isEmpty lz.Left
        //lz |> Option.map (fun x -> List.isEmpty x.Left ) |> Option.defaultValue false
    let isEndL' lz =
        List.isEmpty lz.Right
        //lz |> Option.map (fun x -> List.isEmpty x.Right ) |> Option.defaultValue false
    //let isEmpty lz = lz.State |> Option.isNone
    
    let lzc x = { State = x }
    //let lzc x = Some x |> lzc'
    let ofList xs = 
        { State = Middle, ListZ.ofList xs }
    let bind (f: _ -> _ LZC) lst = lst.State |> f
            // | Some x -> f x
            // | None -> { State = None }
    // let either f g lz =
    //     let fn = function
    //         | Some x -> f x
    //         | None -> g ()
    //     fn lz.State
        // Option.map f
        // >> Option.defaultWith g
    let isSingletone (lzc:_ LZC) =
        lzc.State |> snd
        |> ListZ.isSingleton

    let map f lst = lst.State |> f |> lzc
    let update' f = map (snd >> f)
    
    let endR x = x |> cond isEndR' (comma EndR) (comma Middle)
    let endL x = x |> cond isEndL' (comma EndL) (comma Middle)

    let next lz =
        let toStart' x = ListZ.toStart x |> endR
        lz |> update' (fun xs ->
            ListZ.next xs
            |> Option.map endR
            |> Option.defaultWith (fun () -> toStart' xs))

    let prev lz =
        let toEnd = ListZ.toEnd >> endL
        lz |> update' (fun xs ->
            ListZ.prev xs
            |> Option.map endL
            |> Option.defaultWith (fun () -> toEnd xs))
    let removeR lz =
        lz.State
        |> ( snd >> fun lz ->
            let f x =
                if List.isEmpty lz.Left then
                    (Middle, x) |> lzc |> next
                else
                    endR x |> lzc
            ListZ.removeR lz |> Option.map f)
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


