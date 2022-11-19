namespace FsharpMyExtension
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
            Ok(data, None)
        | None ->
            match WebClientDownloader.getData headers url with
            | Right data ->
                let data = dataMapper data
                let webCacher =
                    { webCacher with
                        Cache = Map.add url data webCacher.Cache
                    }
                Ok(data, Some webCacher)
            | Left errMsg ->
                Error errMsg
