module FsharpMyExtension.Net.ContentType

[<Struct>]
type Typ =
    | Video
    | Text
    | Multipart
    | Message
    | Image
    | Audio
    | Application
    | X of char * char
let typString = function
    | Video            -> "video"
    | Text             -> "text"
    | Multipart        -> "multipart"
    | Message          -> "message"
    | Image            -> "image"
    | Audio            -> "audio"
    | Application      -> "application"
    | X(c1, c2)        -> sprintf "X-%c%c" c1 c2

type Parameter =
    | Charset of System.Text.Encoding
    | CustomParameter of string * string
type ContentType =
    {
        Typ: Typ
        Subtype: string
        Parameter:Parameter option
    }

module Parser =
    open FParsec
    /// https://www.w3.org/Protocols/rfc1341/4_Content-Type.html
    /// ```ebnf
    /// type :=     "application" / "audio"
    ///           / "image"       / "message"
    ///           / "multipart"   / "text"
    ///           / "video"       / x-token
    /// x-token := <The two characters "X-" followed, with no
    ///            intervening white space, by any token>
    /// ```
    let typ : Parser<_,unit> =
        // [
        //     "\"application\" / \"audio\""
        //     "              / \"image\"       / \"message\""
        //     "              / \"multipart\"   / \"text\""
        //     "              / \"video\"       / x-token"
        // ] |> String.concat "\n"
        // |> fun input -> System.Text.RegularExpressions.Regex.Matches(input, "\"(\w+?)\"")
        // |> Seq.cast<System.Text.RegularExpressions.Match> |> Seq.map (fun x -> x.Groups.[1].Value)
        // |> String.concat "\n"
        let dic =
            [
                "video"       , Typ.Video
                "text"        , Typ.Text
                "multipart"   , Typ.Multipart
                "message"     , Typ.Message
                "image"       , Typ.Image
                "audio"       , Typ.Audio
                "application" , Typ.Application
                // x-token
            ] |> List.sortByDescending fst
            // |> List.map (printfn "%A")
            // |> List.map (fun x -> (Seq.head x |> System.Char.ToUpper |> string) + x.[1..] |> printfn "%s" )
            |> List.map (fun (x, y) -> pstring x >>% y)
            |> choice
        let charNotSpace = satisfy ((<>) ' ')
        dic <|> (pstring "X-" >>. charNotSpace .>>. charNotSpace |>> X)

        // pstring "asdf"
    /// ```ebnf
    /// token := 1*&lt;any CHAR except SPACE, CTLs, or tspecials&gt;
    /// tspecials :=  "(" / ")" / "<" / ">" / "@"  ; Must be in
    ///            /  "," / ";" / ":" / "\" / &lt;"&gt;  ; quoted-string,
    ///            /  "/" / "[" / "]" / "?" / "."  ; to use within
    ///            /  "="                          ; parameter values
    /// ```
    let token : Parser<_,unit>  =
        // [
        //     "\"(\" / \")\" / \"<\" / \">\" / \"@\""
        //     "\",\" / \";\" / \":\" / \"\\\" / "
        //     "\"/\" / \"[\" / \"]\" / \"?\" / \".\""
        //     "\"=\""
        // ] |> String.concat "\n"
        // |> fun input -> System.Text.RegularExpressions.Regex.Matches(input, "\"(.)\"")
        // |> Seq.cast<System.Text.RegularExpressions.Match> |> Seq.map (fun x -> x.Groups.[1].Value)
        // |> String.concat ""
        many1Satisfy (isNoneOf "()<>@,;:\\\"/[]?.= \n")
    /// ```ebnf
    /// value := token / quoted-string
    /// attribute := token
    /// parameter := attribute "=" value
    /// ```
    let parameter =
        let value = token // TODO: quoted-string — https://www.w3.org/Protocols/rfc822/3_Lexical.html#z3
        token .>> pchar '=' .>>. value
    /// ```ebnf
    /// Content-Type := type "/" subtype *[";" parameter]
    /// subtype := token
    /// ``
    let contentType : Parser<_,unit> =
        let pcharset =
            pstring "charset" >>. pchar '=' >>. token
            >>= fun x ->
                try
                    let x =
                        match x with
                        | "cp1251" -> "windows-1251"
                        | x -> x
                    let enc = System.Text.Encoding.GetEncoding x
                    preturn (Charset enc)
                with e ->
                    fail e.Message
        let parameter =
            pchar ';'
            >>. spaces >>. (pcharset <|> (parameter |>> CustomParameter))
        pipe3
            (typ .>> pchar '/')
            (token .>> spaces)
            (opt parameter)
            (fun t sub parameter ->
                {
                    Typ = t
                    Subtype = sub
                    Parameter = parameter
                }
            )

    open FsharpMyExtension.Either
    let start rawContentType =
        match run contentType rawContentType with
        | Success(x, _, _) -> Right x
        | Failure(x, _, _) -> Left x
