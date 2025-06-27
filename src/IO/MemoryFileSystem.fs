module FsharpMyExtension.IO.MemoryFileSystem

type Entity =
    | Directory of Map<string, Entity>
    | File of string

type MemoryFileSystem = Map<string, Entity>

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
                |> Ok
            | Some (Directory _) ->
                Error WriteFileError.IsDirectory
        | pathFragment::restPathFragments ->
            match Map.tryFind pathFragment dir with
            | None | Some (File _) ->
                Map.add pathFragment (create restPathFragments content) dir
                |> Ok
            | Some (Directory subDir) ->
                loop restPathFragments subDir
                |> Result.map (fun x ->
                    Map.add pathFragment (Directory x) dir
                )
        | [] ->
            Error WriteFileError.PathFragmentsIsEmpty

    loop pathFragments fileSystem

[<RequireQualifiedAccess>]
type ReadFileError =
    | PathFragmentsIsEmpty
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

    loop pathFragments fileSystem

[<RequireQualifiedAccess>]
type RemoveError =
    | PathFragmentsIsEmpty
    | EntityNotFound

let remove (pathFragments: string list) (fileSystem: MemoryFileSystem) =
    let rec loop pathFragments dir =
        match pathFragments with
        | [pathFragment] ->
            if Map.containsKey pathFragment dir then
                Map.remove pathFragment dir
                |> Directory
                |> Ok
            else
                Error RemoveError.EntityNotFound
        | pathFragment::restPathFragments ->
            match Map.tryFind pathFragment dir with
            | None | Some (File _) ->
                Error RemoveError.EntityNotFound
            | Some (Directory subDir) ->
                loop restPathFragments subDir
                |> Result.map (fun x ->
                    Directory (Map.add pathFragment x dir)
                )
        | [] ->
            Error RemoveError.PathFragmentsIsEmpty

    loop pathFragments fileSystem
