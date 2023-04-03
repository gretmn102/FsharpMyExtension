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

    let gets reqf dataMapper urls (webCacher: WebCacher<'Data>) =
        urls
        |> Seq.map (fun url ->
            match Map.tryFind url webCacher.Cache with
            | Some data ->
                async {
                    return url, Ok {| IsNew = false; Data = data |}
                }
            | None ->
                async {
                    let! x = WebDownloader.tryGet reqf url

                    match x with
                    | Right data ->
                        return url, Ok {| IsNew = true; Data = dataMapper data |}
                    | Left errorValue ->
                        return url, Error errorValue
                }
        )
        |> Async.Parallel
        |> Async.RunSynchronously
        |> fun results ->
            let webCacher =
                results
                |> Array.fold
                    (fun webCacher (url, res) ->
                        match res with
                        | Ok x ->
                            if x.IsNew then
                                { webCacher with
                                    Cache = Map.add url x.Data webCacher.Cache
                                }
                            else
                                webCacher
                        | Error _ ->
                            webCacher
                    )
                    webCacher

            results, webCacher
