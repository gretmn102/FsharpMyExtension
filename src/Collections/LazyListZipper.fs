namespace FsharpMyExtension.Collections
open FsharpMyExtension.ListZipper

type LazyListZipper<'Error, 'a> =
    {
        SrcList: Lazy<LazyList<Result<'a, 'Error>>>
        State: ListZ<'a>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module LazyListZipper =
    [<RequireQualifiedAccess>]
    type 'Error NextResult =
        | EndOfList
        | Error of 'Error

    let create x src =
        {
            SrcList = src
            State = ListZ.singleton x
        }

    let hole (llz: LazyListZipper<'Error,_>) =
        ListZ.hole llz.State

    let next (llz: LazyListZipper<'Error,_>) =
        match ListZ.next llz.State with
        | Some lz ->
            { llz with
                State = lz }
            |> Ok
        | None ->
            match llz.SrcList.Value with
            | Cons(x, xs) ->
                match x with
                | Ok x ->
                    { llz with
                        SrcList = lazy xs.Value
                        State =
                            ListZ.insertAfter x llz.State }
                    |> Ok
                | Error x -> Error (NextResult.Error x)
            | Empty -> Error NextResult.EndOfList

    let prev (llz: LazyListZipper<'Error,_>) =
        match ListZ.prev llz.State with
        | Some lz ->
            { llz with
                State = lz }
            |> Some
        | None -> None
