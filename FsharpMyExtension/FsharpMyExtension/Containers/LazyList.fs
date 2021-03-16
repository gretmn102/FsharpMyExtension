module FsharpMyExtension.LazyList

type LazyList<'a> =
   | Empty
   | Cons of 'a * Lazy<LazyList<'a>>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module LazyList =
    let head = function
        | Cons (h, _) -> h
        | Empty -> failwith "empty list"
    let tail = function
        | Cons (_, t) -> t.Force()
        | Empty -> failwith "empty list"

    let empty = Empty
    let singleton a = Cons (a, lazy ( Empty ))

    /// ## Note
    /// ```
    /// let rec f i =
    ///     LazyList.cons i (f (i + 1))
    /// f 0 // StackOverflow
    /// ```
    /// but:
    /// ```
    /// let rec f i =
    ///     Cons (i, lazy ( f (i + 1) ))
    /// f 0 -> // Ok
    /// ```
    let cons (a : 'a) (l : LazyList<'a>) = Cons (a, lazy ( l ))

    let rec map f = function
        | Cons (a, t) -> Cons (f a, lazy (map f (t.Force())))
        | Empty -> Empty

    let rec iter f = function
        | Cons (a, t) -> f a; iter f (t.Force())
        | Empty -> ()

    let rec take nr = function
        | Cons (a, t) ->
            if nr = 0 then Empty
            else Cons (a, lazy (take (nr-1) (t.Force())))
        | Empty -> Empty

    let rec unfold (f : 's -> ('a*'s) option) (init : 's) : LazyList<'a> =
        match f init with
        | Some (a, s) -> Cons (a, lazy (unfold f s))
        | None -> Empty

    let rec foldr (f : 'a -> Lazy<'s> -> 's) (init : 's) = function
       | Cons (a, t) -> f a (lazy (foldr f init (t.Force())))
       | Empty -> init

    let isEmpty = function
        | Cons _ -> false
        | Empty -> true

    let initInfinite initializer =
        let rec f i =
            Cons (initializer i, lazy ( f (i + 1) ))
        f 0


open FsharpMyExtension.ListZipper
open FsharpMyExtension.Either
type LazyListZipper<'Error, 'a> =
    {
        SrcList: Lazy<LazyList<Either<'Error, 'a>>>
        State: ListZipper.ListZ<'a>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module LazyListZipper =
    type NextResult<'Error, 'a> =
        | EndOfList
        | Error of 'Error

    let create x src =
        {
            SrcList = src
            State = ListZ.singleton x
        }
    let hole (llz:LazyListZipper<'Error,_>) =
        ListZ.hole llz.State

    let next (llz:LazyListZipper<'Error,_>) =
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
                | Left x -> Left (Error x)
            | Empty -> Left EndOfList
    let prev (llz:LazyListZipper<'Error,_>) =
        match ListZ.prev llz.State with
        | Some lz ->
            { llz with
                State = lz }
            |> Some
        | None -> None
