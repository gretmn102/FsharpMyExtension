module FsharpMyExtension.XPathLimited
open FsharpMyExtension

type Req = {
    Name:string option
    Att:(string option * string option) list
    Text:string option
}

// https://www.w3schools.com/xml/xpath_syntax.asp
module Parser =
    open FParsec
    open FsharpMyExtension

    let pname = (pchar '*' >>% None) <|> (manySatisfy (isNoneOf " []=") |>> Some)
    let pval = pchar '=' >>. pchar ''' >>. manySatisfy ((<>) ''') .>> pchar '''
    let patt =
        pchar '@' >>. pname
        >>= fun name ->
            opt pval
            |>> fun att -> name, att
    let ptext = pstring "text()" >>. pval
    let res =
        let brackets p = attempt (pchar '[' >>. p) .>> pchar ']'
        let patts = many (brackets patt)
        pname
        >>= fun name ->
            pipe2 patts (pipe2 (opt (brackets ptext)) patts comma )
                (fun xs (txt, ys) ->
                    let atts =
                        match ys with
                        | [] -> xs
                        | y -> xs @ y
                    { Name = name; Att = atts; Text = txt })
            .>> eof
    open FsharpMyExtension.Either
    let run str =
        match run res str with
        | Success(x, _, _) -> Right x
        | Failure(x, _, _) -> Left x

module ShowReq =
    open FsharpMyExtension.ShowList
    let star x : ShowS =
        x
        |> Option.map showString
        |> Option.defaultValue (showChar '*')
    let showsArg x =
        showAutoParen "'" (showString x) // TODO: экранирование
    let showsAtts xs =
        xs
        |> List.map (fun (name, value) ->
            showAutoParen "[" (
                showChar '@'
                << star name
                << (value
                    |> Option.map (fun x ->
                        showChar '=' << showsArg x)
                    |> Option.defaultValue empty)
            )
        )
        |> joins empty
    let showsText str =
        str
        |> Option.map (fun str ->
            showAutoParen "[" (
                    showString "text()"
                    << showChar '=' << showsArg str)
            )
        |> Option.defaultValue empty
    /// Чтобы обратиться к собственному узлу, нужно преобразовать, к примеру, `a[@atr][text()='a1']` в `self::node()[name()='a'][@atr][text()='a1']`, что функция и делает.
    let showSelf (x:Req) =
        let name =
            showAutoParen "[" (
                x.Name
                |> Option.map (fun name ->
                    showString "name()"
                    << showChar '=' << showsArg name)
                |> Option.defaultValue empty
            )
        showString "self::node()"
        << name << showsAtts x.Att << showsText x.Text
        |> show
    let show (x:Req) =
        star x.Name << showsAtts x.Att << showsText x.Text
        |> show

module HtmlNode =
    let isMatchRaw r (node:HtmlAgilityPack.HtmlNode) =
        let bind next = function
            | None -> next()
            | Some x -> x && next()

        let fn =
            match node.Attributes with
            | null -> fun _ -> false
            | att -> function
                | Some (name:string), None -> att.Contains name
                | Some name, Some v ->
                    match att.Item name with
                    | null -> false
                    | x -> x.Value = v
                | None, Some v -> att |> Seq.exists (fun x -> x.Value = v)
                | None, None -> true

        Option.map ((=) node.Name) r.Name
        |> bind (fun () ->
            if List.forall fn r.Att then
                match r.Text with
                | None -> true
                | Some txt ->
                    let xs = node.ChildNodes
                    if xs.Count = 1 then
                        match xs.[0] with
                        | :? HtmlAgilityPack.HtmlTextNode as x -> x.InnerText = txt
                        | _ -> false
                    else false
            else false)

    open FsharpMyExtension.Either
    let tryIsMatch xpath =
        Parser.run xpath |> Either.map isMatchRaw
    let isMatch xpath =
        tryIsMatch xpath
        |> Either.getOrDef' (failwithf "%A")
