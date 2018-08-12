namespace FsharpMyExtension.XmlBuilder

type Att = string * string
type Node =
    /// При сохранении воспринимается буквально: используется `XmlWriter.WriteRaw`
    | Text    of string
    /// При сохранении используется `XmlWriter.WriteString`, т.е. все управляемые символы экранируются.
    | TextStr of string
    | Node    of string * Att list * Node list
    | Comment of string
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Node =
    open System.IO
    open System.Xml
    let sprint (sb:System.Text.StringBuilder) (x:Node) =
        use sw = new StringWriter(sb)
        use output = new XmlTextWriter(sw, Formatting=Formatting.Indented)
        use writer = XmlWriter.Create(output)
        let rec f = function
            | Node(tag, atts, body) ->
                writer.WriteStartElement tag
                atts |> List.iter writer.WriteAttributeString
                List.iter f body
                writer.WriteEndElement()
            | Text s -> writer.WriteRaw s
            | TextStr s -> writer.WriteString s
            | Comment s -> writer.WriteComment s
        f x
    let sprintNode (x:Node) =
        let output = System.Text.StringBuilder()
        sprint output x
        output.ToString()
    // let sprintNodes (xs:Node seq) =
    //     let output = System.Text.StringBuilder()
    //     xs |> Seq.iter (sprint output)
    //     output.ToString()
    let sprintNodePre preamble (x:Node) =
        let sb = System.Text.StringBuilder()
        sb.AppendLine preamble |> ignore
        sprint sb x
        sb.ToString()        
    /// в начало добавит `<?xml version="1.0"?>`
    let sprintNodeXml (x:Node) =
        sprintNodePre "<?xml version=\"1.0\"?>" x
    let sprintNodeDT (x:Node) =
        sprintNodePre "<!DOCTYPE html>" x
        // let sb = System.Text.StringBuilder()
        // sb.AppendLine "<!DOCTYPE html>" |> ignore
        // sprint sb x
        // sb.ToString()
    let sprintNodeXDT =
        let doctypeXhtml =
            // Не знаю в чем отличия.
            // "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
            "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"
        sprintNodePre doctypeXhtml
    open HtmlAgilityPack
    let ofHtmlNodes =
        let att (node:HtmlNode) =
            node.Attributes |> Seq.map (fun x -> x.Name, x.Value ) |> List.ofSeq
        let rec f (xs:HtmlNode seq) =
            xs |> Seq.choose (fun node ->
                match node.NodeType with
                | HtmlNodeType.Element ->
                    let xs = node.ChildNodes |> f
                    Some <| Node(node.Name, att node, List.ofSeq xs )
                | HtmlNodeType.Text ->
                    if Parser.ParHtmlNode.isEmptyInnerText node then
                        None
                    else
                        let x = node :?> HtmlTextNode
                        Some <| Text x.Text
                | HtmlNodeType.Comment ->
                    // TODO: комментарии всегда отображаются как есть, `XmlWriter.WriteComment` обрамляет их в <!--...-->
                    let x = node :?> HtmlCommentNode
                    Some <| Comment x.Comment
                | x -> failwithf "%A" x
            )
        f
    /// может выдать исключение, если агрумент `HtmlNodeType.Text` типа и при этом пуст или населен обилием `System.Char.IsWhiteSpace` символов.
    let ofHtmlNode =
        Seq.singleton >> ofHtmlNodes >> Seq.head
        
    let toHtmlNode node =
        let d = HtmlDocument()
        d.LoadHtml (sprintNode node)
        d.DocumentNode.FirstChild

    open FsharpMyExtension
    open FsharpMyExtension.ShowList
    open FsharpMyExtension.Either
    let generateHtmlParser (node:Node) =
        let tabs = replicate 4 ' '
        let sub = showString " >>@"
        let next = showString " >>."
        let rec f = function
            | Node(name, atts, body) ->
                let atts =
                    let x =
                        {
                            Parser.XPathPar.Name = Some name
                            Parser.XPathPar.Att = atts |> List.map (mapBoth Some)
                            Parser.XPathPar.Text = None // лучше не надо. Лучше в комментарий вынести текст.
                        }
                    Parser.XPathPar.show x |> showString
                // [
                //     yield showString "takr " << showAutoParen "\"" atts
                //     // let xs = List.collect (f >> List.map ((<<) tabs)) body
                //     let xs = List.collect f body |> List.map ((<<) tabs)
                //     yield! xs
                // ]
                // let xs = List.collect f body |> List.map ((<<) tabs)
                let xs = List.collect f body |> List.map ((<<) tabs)
                let f' x = showString "takr " << showAutoParen "\"" atts << x
                f' (if List.isEmpty xs then next else sub) :: xs
            | Text s | Comment s | TextStr s->
                // List.map (fun x -> showString "// " << showString x)
                //     (List.ofArray (String.lines s))
                List.map ((<<) (showString "// ") << showString)
                    (List.ofArray (String.lines s))
        f node : ShowS list
