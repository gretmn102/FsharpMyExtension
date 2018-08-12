﻿namespace Parser
open FsharpMyExtension
open FsharpMyExtension.FSharpExt

module XPathPar =
    open Primitives
    open FsharpMyExtension.Either

    type Req = { Name:string option; Att:(string option * string option) list; Text:string option }

    let exec r (node:HtmlAgilityPack.HtmlNode) =
        let bind next = function
            | None -> next()
            | Some x -> x && next()
    
        let fn =
            node.Attributes |> function
                | null -> fun _ -> false
                | att -> function
                    | Some (name:string), None -> att.Contains name
                    | Some name, Some v -> match att.Item name with null -> false | x -> x.Value = v
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

    module Parser =
        open ParserString
        open FsharpMyExtension

        //let name = (pchar '*' >>% None) <|> (pstr |>> Some)
        //manyChars (p)
        
        let name = (pchar '*' >>% None) <|> (manySatisfy (isNoneOf " []=") |>> Some)
        let val' = pchar '=' >>. pchar ''' >>. manySatisfy ((<>) ''') .>> pchar '''
        let patt = 
            pchar '@' >>. name >>= fun name -> opt val' |>> fun att -> name,att
        let ptext = pstring "text()" >>. val'
        let attempt x = x
        let res =
            let brackets p = attempt (pchar '[' >>. p) .>> pchar ']'
            let patts = many (brackets patt)
            name >>= fun name ->
            pipe2 patts (pipe2 (opt (brackets ptext)) patts comma )
                (fun xs (txt, ys) -> 
                    let atts = match ys with [] -> xs | y -> xs @ y
                    { Name = name; Att = atts; Text = txt }) .>> pend (sprintf "%A")
    let parse xs = 
        run xs Parser.res |> Either.map (fst >> exec)
    let show (x:Req) = 
        let name = x.Name |> Option.defaultValue "*"
        let atts =
            x.Att
            |> List.map (
                mapPair
                    (Option.defaultValue "*")
                    (Option.map (sprintf "='%s'") >> Option.defaultValue "")
                >> curry (sprintf "@%s%s"))
        
        let all =
            x.Text
            |> Option.map (sprintf "text()=%s" >> fun x -> x::atts)
            |> Option.defaultValue atts
        
        // let xs = 
        List.map (sprintf "[%s]") (all) |> String.concat ""
        |> sprintf "%s%s" name
    
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
    /// empty or contains only System.Char.IsWhiteSpace
    let isEmptyInnerText (node:HtmlNode) =
        let x = node.InnerText
        String.length x = 0 || String.forall System.Char.IsWhiteSpace x
    let textEmpty =
        satisfy (fun node ->
                match HtmlNode.nodeType node with
                | HtmlNodeType.Text -> isEmptyInnerText node
                | _ -> false)
            (fun x -> sprintf "type: %A\ncontent: %s" x.NodeType x.InnerText)
            "text node or empty"
    let br = satisfy (HtmlNode.name >> (=) "br") nodePrint "node with name \"br\""
    let ptext =
        satisfy
            (cond (HtmlNode.nodeType >> (=) HtmlNodeType.Text)
                (not << isEmptyInnerText)
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
    let takr (xpathReq:string) =
        satisfy (XPathPar.parse xpathReq |> Either.getOrDef' (failwithf "%A"))
            nodePrint xpathReq
        .>> ws
    let sub (p:Pars<_,_>) (x:HtmlNode) =
        preturn (run x.ChildNodes (ws >>. p)) |> trav : Pars<_,_>
    let (>>@) p x = p >>= sub x