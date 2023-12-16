namespace FsharpMyExtension.Collections.TreeZipper
open FsharpMyExtension.Collections

type 'a Crumb = 'a * 'a Tree ListZ option
type 'a Breadcrumbs = 'a Crumb list
type 'a TreeZipper = 'a Breadcrumbs
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module TreeZipper =
    let ofTree (Node(x, xs)): _ TreeZipper =
        let xs =
            if List.isEmpty xs then None
            else
                Some <| ListZ.ofList xs
        [x, xs]

    let down (tz: 'a TreeZipper): 'a TreeZipper option =
        match tz with
        | (x, lz) :: xs ->
            lz
            |> Option.map (fun lz ->
                let (Node(y, ys)) = ListZ.hole lz
                if List.isEmpty ys then
                    (y, None)::(x, ListZ.removeR lz )::xs
                else
                    (y, Some <| ListZ.ofList ys)::(x, ListZ.removeR lz )::xs
            )
        | [] ->
            failwith "empty TreeZipper"

    let up (tz: 'a TreeZipper): 'a TreeZipper =
        match tz with
        | (x, lz)::(y, lz')::xs ->
            let lz =
                lz
                |> Option.map ListZ.toList
                |> Option.defaultValue []

            let lz =
                lz'
                |> Option.map (ListZ.insertAfter (Node(x, lz)))
                |> Option.defaultWith (fun () -> ListZ.singleton (Node(x, lz)))

            (y, Some lz)::xs
        | xs ->
            failwithf "expected _::_::xs but %A" xs

    let next (tz: 'a TreeZipper): 'a TreeZipper option =
        match tz with
        | (x, lz)::xs ->
            lz
            |> Option.bind
                (ListZ.next
                    >> Option.map (fun lz -> (x, Some lz) :: xs)
                )
        | [] -> None

    let append y (tz: 'a TreeZipper) : 'a TreeZipper =
        match tz with
        | (x, lz)::xs ->
            let y = Node(y, [])

            let lz =
                lz
                |> Option.map (ListZ.insertAfter y)
                |> Option.defaultWith (fun () -> ListZ.singleton y)

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
                xs |> Option.map ListZ.toList
                |> Option.defaultValue []
            Node(x, xs)
        | x -> up x |> toTree
