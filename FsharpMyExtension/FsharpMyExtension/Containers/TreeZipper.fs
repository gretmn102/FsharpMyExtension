namespace FsharpMyExtension.TreeZipper
open FsharpMyExtension.Tree
open FsharpMyExtension.ListZipper

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
        | [] -> failwith "now is already now"

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

    // let sample =
    //     // [
    //         Node("a", [
    //             Node("b", [])
    //             Node("c", [
    //                 Node("d", [
    //                     Node("e", [])
    //                 ])
    //                 Node("f", [])
    //             ])
    //             Node("g", [])
    //         ])
    //         // Node("h", [])
    //     // ]
    // let head = ofTree sample

    // next head
    // |> Option.bind down
    // |> Option.map (update (fun _ -> "update"))
    // |> Option.bind down
    // |> Option.map (update (fun _ -> "d update"))
    // |> Option.map goUp
    // |> Option.map goUp

    // // []
    // // ("some", ListZ.ofList [Node("x", [])])::head
    // // |> goUp
    // down head
    // |> Option.map goUp
    // |> Option.bind down

    // let appendAndDown x = append x >> down >> Option.get
    // [("some", None)]
    // |> appendAndDown (Node("one", []))
    // |> append (Node("two", []))
    // |> append (Node("three", []))
    // |> goUp
