module FsharpMyExtension.Path
let changeFileNameWithoutExt fname path =
    [|
        match System.IO.Path.GetDirectoryName path with
        | "" -> ()
        | path -> yield path; yield "\\"
        yield fname <| System.IO.Path.GetFileNameWithoutExtension path
        yield System.IO.Path.GetExtension path
    |] |> System.String.Concat