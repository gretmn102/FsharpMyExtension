namespace Parser
open FsharpMyExtension
open FsharpMyExtension.FSharpExt


module ParHtmlNode =
    open Primitives
    open FsharpMyExtension
    open FsharpMyExtension.HtmlAgilityPackExt
    open FsharpMyExtension.Either
    open HtmlAgilityPack

    let private satisfy f note noteExpected =
        satisfy f note noteExpected : Pars<HtmlNode,_>

    let nodePrint (node:HtmlNode) =
        match node.NodeType with
        | HtmlNodeType.Text ->
            sprintf "htmlText, contain:\n'%s'" (HtmlNode.innerText node)
        | _ ->
            sprintf "<%s %s>\n%s\n</%s>"
                (HtmlNode.name node)
                (node.Attributes
                    |> Seq.map (fun x -> sprintf "%s='%s'" x.Name x.Value)
                    |> String.concat " ")
                node.InnerHtml
                node.Name
    let textEmpty =
        satisfy (fun node ->
                match HtmlNode.nodeType node with
                | HtmlNodeType.Text -> HtmlNode.IsNullOrWhiteSpace node
                | _ -> false)
            (fun x -> sprintf "type: %A\ncontent: %s" x.NodeType x.InnerText)
            "text node or empty"
    let br = satisfy (HtmlNode.name >> (=) "br") nodePrint "node with name \"br\""
    let ptext =
        satisfy
            (cond (HtmlNode.nodeType >> (=) HtmlNodeType.Text)
                (not << HtmlNode.IsNullOrWhiteSpace)
                (k false))
            nodePrint "text node"

    let pcomm =
        satisfy (HtmlNode.nodeType >> (=) HtmlNodeType.Comment)
            nodePrint
            "comment node"

    let takeNC name content =
        satisfy (fun x -> HtmlNode.name x = name && HtmlNode.innerText x = content)
            (sprintf "expected node with name: '%s' and content:\n'%s', but take %A"
                name content)
    let takeN name =
        satisfy (fun x -> x.Name = name)
            (sprintf "expected node with name: '%s', but take %A" name)
    let takeC content =
        satisfy (fun x -> HtmlNode.innerText x = content)
            (sprintf "expected node with content:\n'%s', but take %A" content)

    let ws = many textEmpty
    let takr (xpath:string) =
        satisfy (XPathLimited.HtmlNode.isMatch xpath) nodePrint xpath
        .>> ws
    let sub (p:Pars<_,_>) (x:HtmlNode) =
        preturn (run x.ChildNodes (ws >>. p)) |> trav : Pars<_,_>
    let (>>@) p x = p >>= sub x
module ParHtmlNode2 =
    open Primitives2
    open FsharpMyExtension
    open FsharpMyExtension.HtmlAgilityPackExt
    open FsharpMyExtension.Either
    open HtmlAgilityPack

    let inline textEmpty<'u> : Pars<_, _, 'u>  =
        satisfy (fun node ->
                match HtmlNode.nodeType node with
                | HtmlNodeType.Text -> HtmlNode.IsNullOrWhiteSpace node
                | _ -> false)
            "empty text node"
    let inline br<'u> : Pars<_, _, 'u>  =
        satisfy (HtmlNode.name >> (=) "br") "<br>"

    let inline ptext<'u> : Pars<_, _, 'u> =
        satisfy
            (cond (HtmlNode.nodeType >> (=) HtmlNodeType.Text)
                (not << HtmlNode.IsNullOrWhiteSpace)
                (k false))
            "not empty text node"

    let pcomm<'u> : Pars<_, _, 'u>  =
        satisfy (HtmlNode.nodeType >> (=) HtmlNodeType.Comment)
            "comment node"

    let takeNC name content =
        satisfy (fun (x:HtmlNode) ->
            x.NodeType = HtmlAgilityPack.HtmlNodeType.Element
            && HtmlNode.name x = name
            && HtmlNode.innerText x = content)
            (sprintf "<%s>%s</%s>" name content name)
    let takeN name =
        satisfy (fun (x:HtmlNode) ->
            x.NodeType = HtmlAgilityPack.HtmlNodeType.Element && x.Name = name)
            (sprintf "<%s>" name)
    let takeC content =
        satisfy (fun (x:HtmlNode) ->
            x.NodeType = HtmlAgilityPack.HtmlNodeType.Element
            && HtmlNode.innerText x = content)
            (sprintf "<*>%s</*>" content)
    let inline ws<'u> : Pars<_, _, 'u>  = many textEmpty
    let takr (xpath:string) =
        satisfy (XPathLimited.HtmlNode.isMatch xpath) xpath
        .>> ws
    let sub (p:Pars<_,_,_>) (x:HtmlNode, st) =
        preturn (runs x.ChildNodes st (ws >>. p)) |> trav : Pars<_,_,_>
    let (>>@) p x = p .>>. getUserState >>= sub x

    let (/*) p x = p .>>. getUserState >>= sub (x ())

    open FsharpMyExtension.XmlBuilder
    let run p nodes =
        let (xs, _), res =
            Parser.Primitives2.run nodes p
        match res with
        | Right x -> Right x
        | Left x ->
            let print xs =
                xs
                |> Node.ofHtmlNodes
                |> Seq.map Node.sprintNode
                |> String.concat "\n"
            sprintf "%s\n***\n%s\n%A" (print nodes) (print xs) x
            |> Left

    // Tree.Tree.visualize (sprintf "%s")

    open FsharpMyExtension
    open FsharpMyExtension.XmlBuilder
    open FsharpMyExtension.ShowList
    open FsharpMyExtension.Either
    let generateHtmlParser (node:Node) =
        let tab = replicate 4 ' '
        let sub = showString "/* fun () ->"
        let nextOpName = ".>>."
        let next = showString nextOpName
        let rec f isSecond xs =
            let showText (name:string) str =
                let next = if isSecond then next << showSpace else id
                match List.ofArray (String.lines str) with
                | x::xs ->
                    let x =
                        next
                        << showString name
                        << showSpace << showString "//"
                        << showSpace << showString x
                    let xs =
                        let showIdent =
                            let count =
                                if isSecond then
                                    nextOpName.Length + 1 + name.Length + 1
                                else
                                    name.Length + 1
                            replicate count ' '
                        xs
                        |> List.map (fun x ->
                            showIdent << showString "//"
                            << showSpace << showString x)
                    x :: xs
                | _ -> []
            match xs with
            | Node(name, atts, body) ->
                let atts =
                    let x =
                        {
                            XPathLimited.Name = Some name
                            XPathLimited.Att = atts |> List.map (mapBoth Some)
                            XPathLimited.Text = None
                        }
                    XPathLimited.ShowReq.show x |> showString
                let next = if isSecond then next << showSpace else id
                let xs =
                    match body with
                    | [x] ->
                        f false x
                    | x::xs ->
                        f false x @ List.collect (f true) xs
                    | [] -> []
                    |> List.map ((<<) tab)
                if List.isEmpty xs then
                    next << showString "takr"
                    << showSpace << showAutoParen "\"" atts
                    |> List.singleton
                else
                    next << showString "takr"
                    << showSpace << showAutoParen "\"" atts
                    << showSpace << sub
                    |> fun x -> x :: xs
            | Text s | TextStr s ->
                showText "ptext" s
            | Comment s ->
                showText "pcomm" s
        f false node : ShowS list