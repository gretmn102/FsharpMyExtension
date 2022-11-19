module FsharpMyExtension.WebCache
open FsharpMyExtension
open FsharpMyExtension.Either

type WebCacher<'Data> =
    {
        Cache: Map<string, 'Data>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module WebCacher =
    let empty: WebCacher<'Data> =
        {
            Cache = Map.empty
        }

    let get headers dataMapper url (webCacher: WebCacher<'Data>) =
        match Map.tryFind url webCacher.Cache with
        | Some data ->
            Ok data
        | None ->
            match WebClientDownloader.getData headers url with
            | Right data ->
                dataMapper data
                |> Ok
            | Left errMsg ->
                Error errMsg
