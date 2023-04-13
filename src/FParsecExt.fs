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

let pbigint<'UserState> : Primitives.Parser<_, 'UserState> =
    let digitsToNum xs =
        xs
        |> List.rev
        |> List.fold (fun (acc, i) x ->
            (acc + x * pown 10I i, i + 1)
             ) (0I, 0)
        |> fst
    many1 digit |>> (List.map (fun d -> bigint (int d - 48)) >> digitsToNum)

module ParserResult =
    let toResult (parserResult: ParserResult<_,_>) =
        match parserResult with
        | Success(res, userState, pos) -> Result.Ok (res, userState, pos)
        | Failure(errMsg, parserError, userState) -> Result.Error (errMsg, parserError, userState)

let runParserOnSubstringStart p startIndex str =
    runParserOnSubstring p () "" str startIndex (str.Length - startIndex)
