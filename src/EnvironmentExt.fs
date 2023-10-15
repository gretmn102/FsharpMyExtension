[<RequireQualifiedAccess>]
module FsharpMyExtension.EnvironmentExt.Environment
open System.Collections.Generic
open dotenv.net

// todo: move to DictionaryExt
[<RequireQualifiedAccess>]
module Dictionary =
    let tryGetValue key (dic: #IDictionary<_, _>) =
        match dic.TryGetValue key with
        | true, value -> Some value
        | false, _ -> None

[<RequireQualifiedAccess>]
type LocalEnvironmentsError =
    | DotEnvNotExists
    | SomeOtherError of exn

[<RequireQualifiedAccess>]
type LocalEnvironments =
    | HasNotLoadedYet
    | Resolved of IDictionary<string, string>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module LocalEnvironments =
    let internal load' () =
        try
            let x = DotEnv.Fluent()
            x.Load()
            x.Read()
            |> Ok
        with e ->
            Error (LocalEnvironmentsError.SomeOtherError e)

    let load () =
        match load'() with
        | Ok locals ->
            Ok (LocalEnvironments.Resolved locals)
        | Error(errorValue) ->
            Error errorValue

    let tryGetValue envVar (state: LocalEnvironments) =
        match state with
        | LocalEnvironments.HasNotLoadedYet ->
            match load'() with
            | Ok locals ->
                let result = Dictionary.tryGetValue envVar locals
                Ok result, LocalEnvironments.Resolved locals

            | Error(errorValue) ->
                Error errorValue, LocalEnvironments.HasNotLoadedYet

        | LocalEnvironments.Resolved locals as state ->
            let result = Dictionary.tryGetValue envVar locals
            Ok result, state

[<RequireQualifiedAccess>]
module MutableLocalEnvironments =
    [<RequireQualifiedAccess>]
    type internal Action =
        | TryGet of string * AsyncReplyChannel<Result<string option, LocalEnvironmentsError>>
        | Reload of AsyncReplyChannel<Result<unit, LocalEnvironmentsError>>

    let internal reduce action (state: LocalEnvironments) =
        match action with
        | Action.TryGet(envVar, r) ->
            let result, state =
                LocalEnvironments.tryGetValue envVar state
            r.Reply result
            state

        | Action.Reload r ->
            let result, state =
                match LocalEnvironments.load () with
                | Ok state ->
                    Ok (), state
                | Error error ->
                    Error error, LocalEnvironments.HasNotLoadedYet
            r.Reply result
            state

    let internal m =
        MailboxProcessor.Start(fun mail ->
            let rec loop (state: LocalEnvironments) =
                async {
                    let! action = mail.Receive()
                    let state =
                        try
                            reduce action state
                        with e ->
                            let error = Error (LocalEnvironmentsError.SomeOtherError e)
                            match action with
                            | Action.TryGet(_, r) ->
                                r.Reply error
                            | Action.Reload r ->
                                r.Reply error
                            state
                    return! loop state
                }
            loop LocalEnvironments.HasNotLoadedYet
        )

    let reload () = m.PostAndReply(fun r -> Action.Reload r)

    let tryGet envVar = m.PostAndReply(fun r -> Action.TryGet(envVar, r))

/// First function tries to read the environment variable from `.env`,
/// and if there is none, it reads from the global environment.
/// Does not throw an exception if the `.env` file is missing.
let tryGetEnvironmentVariable envVar =
    match MutableLocalEnvironments.tryGet envVar with
    | Error _ | Ok None ->
        match System.Environment.GetEnvironmentVariable envVar with
        | null -> None
        | value -> Some value
    | Ok(Some value) ->
        Some value

/// First function read the environment variable from `.env`,
/// and if there is none, it reads from the global environment.
/// Does not throw an exception if the `.env` file is missing.
let getEnvironmentVariable varName =
    tryGetEnvironmentVariable varName
    |> Option.defaultWith (fun () ->
        failwithf "Environment variable `%s` is not set!" varName
    )
