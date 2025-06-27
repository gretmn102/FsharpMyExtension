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
