namespace FsharpMyExtension.Collections

type LazyListZipper<'Error, 'a> =
    {
        SrcList: Lazy<LazyList<Result<'a, 'Error>>>
        State: ListZipper<'a>
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
            State = ListZipper.singleton x
        }

    let hole (llz: LazyListZipper<'Error,_>) =
        ListZipper.hole llz.State

    let next (llz: LazyListZipper<'Error,_>) =
        match ListZipper.next llz.State with
        | Some lz ->
            { llz with
                State = lz }
            |> Ok
        | None ->
            match llz.SrcList.Value with
            | LazyList.Cons(x, xs) ->
                match x with
                | Ok x ->
                    { llz with
                        SrcList = lazy xs.Value
                        State =
                            ListZipper.insertAfter x llz.State }
                    |> Ok
                | Error x -> Error (NextResult.Error x)
            | LazyList.Empty -> Error NextResult.EndOfList

    let prev (llz: LazyListZipper<'Error,_>) =
        match ListZipper.prev llz.State with
        | Some lz ->
            { llz with
                State = lz }
            |> Some
        | None -> None
