module FsharpMyExtension.Path
open System.IO

open FsharpMyExtension.Primitives

let changeFileNameWithoutExt fname path =
    [|
        match System.IO.Path.GetDirectoryName path with
        | "" -> ()
        | path -> yield path; yield "\\"
        yield fname <| System.IO.Path.GetFileNameWithoutExtension path
        yield System.IO.Path.GetExtension path
    |] |> System.String.Concat

let escapingFileName =
    let xs =
        System.IO.Path.GetInvalidFileNameChars()
        |> Set.ofArray
    String.filter (flip Set.contains xs >> not)

/// Строит относительный путь из текущей папки к внешнему файлу или папке.
/// Все пути должны быть абсолютными, иначе ничего не получится.
/// Разделитель путей — `\`
/// Примеры — в тестовом файле.
let relative path dir =
    let dir =
        if System.IO.Path.GetExtension dir = "" then
            let last = String.last dir
            if last = '\\' then
                dir.[..dir.Length - 2]
            else
                dir
        else
            System.IO.Path.GetDirectoryName dir

    let rec f = function
        | x::xs, y::ys ->
            if x = y then
                f (xs, ys)
            else
                List.replicate (List.length (x::xs)) ".." @ (y::ys)
        | _, ys -> ys
    (dir, path)
    |> mapBoth (String.split "\\" >> List.ofArray)
    |> f
    |> String.concat "\\"

/// Works like `System.IO.Path.GetExtension`, but does not check through `System.IO.Path.GetInvalidPathChars`
let getExtension (str:string) =
    let rec f i =
        if i < 0 then -1
        else
            if str.[i] = '.' then i
            else
                f (i - 1)
    let i = f (str.Length - 1)
    if i < 0 then ""
    else
        str.[i..]

let changeExt ext path =
    [|
        Path.GetDirectoryName path
        string Path.DirectorySeparatorChar
        Path.GetFileNameWithoutExtension path
        ext
    |]
    |> System.String.Concat
