namespace FsharpMyExtension.Collections

[<RequireQualifiedAccess>]
type LazyList<'a> =
   | Empty
   | Cons of 'a * Lazy<LazyList<'a>>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module LazyList =
    let head = function
        | LazyList.Cons (h, _) -> h
        | LazyList.Empty -> failwith "empty list"

    let tail = function
        | LazyList.Cons (_, t) -> t.Force()
        | LazyList.Empty -> failwith "empty list"

    let empty = LazyList.Empty

    let singleton a = LazyList.Cons (a, lazy ( LazyList.Empty ))

    /// ## Note
    /// ```
    /// let rec f i =
    ///     LazyList.cons i (f (i + 1))
    /// f 0 // StackOverflow
    /// ```
    /// but:
    /// ```
    /// let rec f i =
    ///     LazyList.Cons (i, lazy ( f (i + 1) ))
    /// f 0 -> // Ok
    /// ```
    let cons (a : 'a) (l : LazyList<'a>) = LazyList.Cons (a, lazy ( l ))

    let rec map f = function
        | LazyList.Cons (a, t) ->
            LazyList.Cons (f a, lazy (map f (t.Force())))
        | LazyList.Empty ->
            LazyList.Empty

    let rec iter f = function
        | LazyList.Cons (a, t) ->
            f a
            iter f (t.Force())
        | LazyList.Empty -> ()

    let rec take nr = function
        | LazyList.Cons (a, t) ->
            if nr = 0 then
                LazyList.Empty
            else
                LazyList.Cons (a, lazy (take (nr-1) (t.Force())))
        | LazyList.Empty -> LazyList.Empty

    let rec unfold (f : 's -> ('a * 's) option) (init : 's) : LazyList<'a> =
        match f init with
        | Some (a, s) -> LazyList.Cons (a, lazy (unfold f s))
        | None -> LazyList.Empty

    let rec foldr (f : 'a -> Lazy<'s> -> 's) (init : 's) = function
       | LazyList.Cons (a, t) -> f a (lazy (foldr f init (t.Force())))
       | LazyList.Empty -> init

    let isEmpty = function
        | LazyList.Cons _ -> false
        | LazyList.Empty -> true

    let initInfinite initializer =
        let rec f i =
            LazyList.Cons (initializer i, lazy ( f (i + 1) ))
        f 0
