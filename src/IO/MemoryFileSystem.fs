module FsharpMyExtension.IO.MemoryFileSystem

type MemoryFileSystem =
    | Directory of Map<string, MemoryFileSystem>
    | File of string

let rec create (pathFragments: string list) content =
    Directory (
        match pathFragments with
        | [] -> failwith "pathFragments is empty!"
        | [fileName] ->
            Map [fileName, File content]
        | pathFragment::pathFragments ->
            Map [pathFragment, create pathFragments content]
    )

[<RequireQualifiedAccess>]
type WriteFileError =
    | IsDirectory
    | PathFragmentsIsEmpty
    | NotImplementedYet

let writeFile
    (pathFragments: string list)
    content
    (fileSystem: MemoryFileSystem)
    : Result<MemoryFileSystem, WriteFileError> =
    let rec loop pathFragments fileSystem =
        match pathFragments with
        | [fileName] ->
            match fileSystem with
            | Directory dir ->
                match Map.tryFind fileName dir with
                | None | Some (File _) ->
                    Map.add fileName (File content) dir
                    |> Directory
                    |> Ok
                | Some (Directory _) ->
                    Error WriteFileError.IsDirectory
            | File _ ->
                Map.add fileName (File content) Map.empty
                |> Directory
                |> Ok
        | pathFragment::restPathFragments ->
            match fileSystem with
            | Directory dir ->
                match Map.tryFind pathFragment dir with
                | None | Some (File _) ->
                    Map.add pathFragment (create restPathFragments content) dir
                    |> Directory
                    |> Ok
                | Some (Directory _ as subDir) ->
                    loop restPathFragments subDir
                    |> Result.map (fun x ->
                        Directory (Map.add pathFragment x dir)
                    )
            | File _ ->
                failwith "не должно случиться?"
        | [] ->
            Error WriteFileError.PathFragmentsIsEmpty
    loop pathFragments fileSystem
