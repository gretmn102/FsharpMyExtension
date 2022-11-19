namespace FsharpMyExtension.ResultExt

module Result =
    let isOk (result: Result<'Ok, 'Error>) =
        match result with
        | Ok _ -> true
        | _ -> false

    let isError (result: Result<'Ok, 'Error>) =
        match result with
        | Error _ -> true
        | _ -> false

    let get (result: Result<'Ok, 'Error>) =
        match result with
        | Ok x -> x
        | Error x ->
            failwithf "Expected Ok but get Error %A" x

    let getError (result: Result<'Ok, 'Error>) =
        match result with
        | Error x -> x
        | Ok x ->
            failwithf "Expected Error but get Ok %A" x

[<RequireQualifiedAccess>]
module List =
    let travResult (fn: 'a -> Result<'b, 'Error>) (xs:'a list): Result<'b list, 'Error> =
        let rec f acc = function
            | x::xs ->
                match fn x with
                | Ok x -> f (x::acc) xs
                | Error x -> Error x
            | [] -> Ok (List.rev acc)
        f [] xs

    let seqResult (xs: Result<'Ok, 'Error> list) =
        travResult id xs

    let partitionResults (xs: Result<'Ok, 'Error> list) =
        xs
        |> List.partition (Result.isOk)
        |> fun (xs, ys) -> List.map Result.getError xs, List.map Result.get ys
