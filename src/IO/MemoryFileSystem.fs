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
    | FileSystemStartAsFile

let writeFile
    (pathFragments: string list)
    content
    (fileSystem: MemoryFileSystem)
    : Result<MemoryFileSystem, WriteFileError> =
    let rec loop pathFragments dir =
        match pathFragments with
        | [fileName] ->
            match Map.tryFind fileName dir with
            | None | Some (File _) ->
                Map.add fileName (File content) dir
                |> Directory
                |> Ok
            | Some (Directory _) ->
                Error WriteFileError.IsDirectory
        | pathFragment::restPathFragments ->
            match Map.tryFind pathFragment dir with
            | None | Some (File _) ->
                Map.add pathFragment (create restPathFragments content) dir
                |> Directory
                |> Ok
            | Some (Directory subDir) ->
                loop restPathFragments subDir
                |> Result.map (fun x ->
                    Directory (Map.add pathFragment x dir)
                )
        | [] ->
            Error WriteFileError.PathFragmentsIsEmpty

    match fileSystem with
    | Directory dir ->
        loop pathFragments dir
    | File _ ->
        Error WriteFileError.FileSystemStartAsFile

[<RequireQualifiedAccess>]
type ReadFileError =
    | PathFragmentsIsEmpty
    | FileSystemStartAsFile
    | FileNotFound
    | IsDirectory

let readFile (pathFragments: string list) (fileSystem: MemoryFileSystem) =
    let rec loop pathFragments dir =
        match pathFragments with
        | [pathFragment] ->
            match Map.tryFind pathFragment dir with
            | Some entity ->
                match entity with
                | File content -> Ok content
                | Directory _ -> Error ReadFileError.IsDirectory
            | None -> Error ReadFileError.FileNotFound
        | pathFragment::restPathFragments ->
            match Map.tryFind pathFragment dir with
            | None | Some (File _) ->
                Error ReadFileError.FileNotFound
            | Some (Directory dir) ->
                loop restPathFragments dir
        | [] ->
            Error ReadFileError.PathFragmentsIsEmpty

    match fileSystem with
    | Directory dir ->
        loop pathFragments dir
    | File _ -> Error ReadFileError.FileSystemStartAsFile
