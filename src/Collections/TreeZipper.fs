namespace FsharpMyExtension.Collections
open FsharpMyExtension.Collections

type 'a TreeZipperCrumb = 'a * 'a Tree ListZipper option
type 'a TreeZipperBreadcrumbs = 'a TreeZipperCrumb list
type 'a TreeZipper = 'a TreeZipperBreadcrumbs
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module TreeZipper =
    let ofTree (Node(x, xs)): _ TreeZipper =
        let xs =
            if List.isEmpty xs then None
            else
                Some <| ListZipper.ofList xs
        [x, xs]

    let down (tz: 'a TreeZipper): 'a TreeZipper option =
        match tz with
        | (x, lz) :: xs ->
            lz
            |> Option.map (fun lz ->
                let (Node(y, ys)) = ListZipper.hole lz
                if List.isEmpty ys then
                    (y, None)::(x, ListZipper.removeR lz )::xs
                else
                    (y, Some <| ListZipper.ofList ys)::(x, ListZipper.removeR lz )::xs
            )
        | [] ->
            failwith "empty TreeZipper"

    let up (tz: 'a TreeZipper): 'a TreeZipper =
        match tz with
        | (x, lz)::(y, lz')::xs ->
            let lz =
                lz
                |> Option.map ListZipper.toList
                |> Option.defaultValue []

            let lz =
                lz'
                |> Option.map (ListZipper.insertAfter (Node(x, lz)))
                |> Option.defaultWith (fun () -> ListZipper.singleton (Node(x, lz)))

            (y, Some lz)::xs
        | xs ->
            failwithf "expected _::_::xs but %A" xs

    let next (tz: 'a TreeZipper): 'a TreeZipper option =
        match tz with
        | (x, lz)::xs ->
            lz
            |> Option.bind
                (ListZipper.next
                    >> Option.map (fun lz -> (x, Some lz) :: xs)
                )
        | [] -> None

    let append y (tz: 'a TreeZipper) : 'a TreeZipper =
        match tz with
        | (x, lz)::xs ->
            let y = Node(y, [])

            let lz =
                lz
                |> Option.map (ListZipper.insertAfter y)
                |> Option.defaultWith (fun () -> ListZipper.singleton y)

            (x, Some lz)::xs
        | [] ->
            failwith "treezipper is empty"

    let update f (bs: 'a TreeZipper): 'a TreeZipper =
        match bs with
        | (x, lz)::xs -> (f x, lz)::xs
        | [] -> failwith "empty"

    let rec toTree (tz:_ TreeZipper) =
        match tz with
        | [x, xs] ->
            let xs =
                xs |> Option.map ListZipper.toList
                |> Option.defaultValue []
            Node(x, xs)
        | x -> up x |> toTree
