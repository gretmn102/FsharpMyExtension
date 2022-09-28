module FsharpMyExtension.FParsecExt
open FParsec

open FsharpMyExtension.Either

let runEither p str =
    match run p str with
    | Success(res, _, _) -> Right res
    | Failure(errMsg, _, _) -> Left errMsg

let runResult p str =
    match run p str with
    | Success(res, _, _) -> Result.Ok res
    | Failure(errMsg, _, _) -> Result.Error errMsg
