namespace FsharpMyExtension

/// https://en.wikipedia.org/wiki/Bencode
[<RequireQualifiedAccess>]
type Bencode =
    | Int of int
    | String of string
    | Dictionary of Map<string, Bencode>
    | List of Bencode list
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Bencode =
    let dic keyValues =
        Bencode.Dictionary (Map.ofList keyValues)

    let int =
        Bencode.Int

    let str =
        Bencode.String

    let list =
        Bencode.List

    module Show =
        open ShowList

        let showEnd =
            showChar 'e'

        let showInt n : ShowS =
            showChar 'i'
            << shows n
            << showEnd

        let showString (str: string) : ShowS =
            shows str.Length
            << showChar ':'
            << showString str

        let showList (showBencodes: Bencode -> ShowS) (xs: Bencode list) : ShowS =
            showChar 'l'
            << (xs |> List.map showBencodes |> joinsEmpty id)
            << showEnd

        let showDictionary (showBencodes: Bencode -> ShowS) (xs: Map<string, Bencode>) : ShowS =
            let showKeyValue (KeyValue(k, v)) =
                showString k
                << showBencodes v

            showChar 'd'
            << (xs |> Seq.map showKeyValue |> List.ofSeq |> joinsEmpty id)
            << showEnd

        let rec show (bencode: Bencode) =
            match bencode with
            | Bencode.String str ->
                showString str
            | Bencode.Int n ->
                showInt n
            | Bencode.List xs ->
                showList show xs
            | Bencode.Dictionary xs ->
                showDictionary show xs

    let serialize x =
        Show.show x |> ShowList.show

    module Parser =
        open FParsec
        open FParsecExt

        type Parser<'T> = Parser<'T, unit>

        let pstring: _ Parser =
            pint32 .>> skipChar ':' >>= fun length ->
            anyString length

        let pend: _ Parser = skipChar 'e'

        let pint: _ Parser =
            pchar 'i' >>. pint32 .>> pend

        let pdic expr =
            let pkeyValue expr =
                tuple2 pstring expr

            skipChar 'd' >>. (many (pkeyValue expr) |>> Map.ofList) .>> pend

        let plist expr =
            skipChar 'l' >>. (many expr) .>> pend

        let parser: _ Parser =
            let parser, refParser =
                createParserForwardedToRef()

            refParser :=
                choice [
                    pstring |>> Bencode.String
                    pint |>> Bencode.Int
                    pdic parser |>> Bencode.Dictionary
                    plist parser |>> Bencode.List
                ]
            parser

    let deserialize raw =
        FParsecExt.runResult Parser.parser raw
