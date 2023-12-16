module FsharpMyExtension.Collections.LazyListZipper
open FsharpMyExtension.ListZipper
open FsharpMyExtension.Either
open FsharpMyExtension.LazyList

type LazyListZipper<'Error, 'a> =
    {
        SrcList: Lazy<LazyList<Either<'Error, 'a>>>
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
            |> Right
        | None ->
            match llz.SrcList.Value with
            | Cons(x, xs) ->
                match x with
                | Right x ->
                    { llz with
                        SrcList = lazy xs.Value
                        State =
                            ListZ.insertAfter x llz.State }
                    |> Right
                | Left x -> Left (NextResult.Error x)
            | Empty -> Left NextResult.EndOfList

    let prev (llz: LazyListZipper<'Error,_>) =
        match ListZ.prev llz.State with
        | Some lz ->
            { llz with
                State = lz }
            |> Some
        | None -> None
