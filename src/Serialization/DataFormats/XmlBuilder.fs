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
        use writer = new XmlTextWriter(sw, Formatting=Formatting.Indented)
        let rec f = function
            | Node(tag, atts, body) ->
                writer.WriteStartElement tag
                atts
                |> List.iter (fun (name, value) ->
                    writer.WriteStartAttribute(name)
                    value
                    |> String.collect (function
                        | '"' -> "&quot;"
                        | x -> string x )
                    |> writer.WriteRaw
                    writer.WriteEndAttribute()
                )
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
    /// обязательно должно быть: `<html xmlns="http://www.w3.org/1999/xhtml">`
    let sprintNodeXDT =
        let doctypeXhtml =
            // Не знаю в чем отличия.
            // "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
            "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"
        sprintNodePre doctypeXhtml

    open HtmlAgilityPack
    open FsharpMyExtension.Serialization.DataFormats.HtmlAgilityPackExt

    /// Осторожно, некоторая информация теряется при переносе, например:
    /// ```html
    /// <p>пробел<!-- потерян --> <b>здесь</b></p>
    /// <p>пробел<!-- потерян --><b>здесь</b></p>
    /// ```
    /// Код, чтобы воссоздать:
    /// ```fsharp
    /// let exp = "<p>пробел<!-- потерян --> <b>здесь</b></p>"
    /// let htmlNode = HtmlNode.ofString exp
    /// let act =
    ///     Node.ofHtmlNode htmlNode.FirstChild
    ///     |> Node.sprintNode
    /// exp = act
    /// ```
    let ofHtmlNodes =
        let att (node:HtmlNode) =
            node.Attributes |> Seq.map (fun x -> x.Name, x.Value ) |> List.ofSeq
        let rec f (xs:HtmlNode seq) =
            xs
            |> Seq.choose (fun node ->
                match node.NodeType with
                | HtmlNodeType.Element ->
                    let xs = node.ChildNodes |> f
                    Some <| Node(node.Name, att node, List.ofSeq xs )
                | HtmlNodeType.Text ->
                    if HtmlNode.IsNullOrWhiteSpace node then
                        None
                    else
                        let x = node :?> HtmlTextNode
                        Some <| Text x.Text
                | HtmlNodeType.Comment ->
                    // `HtmlCommentNode` устроен так, что его значения записываются в html как есть: то есть если `HtmlCommentNode.Comment = "<!-- comment -->"`, то и в html он таким запишется как `<!-- comment -->`, а если `HtmlCommentNode.Comment = "comment"`, то html на выходе получится как `comment`. Одна из причин подобного — "<!DOCTYPE html>", ведь не понятно, куда его деть, и авторы вставили его в `HtmlCommentNode.Comment` без обрамления.
                    let x = (node :?> HtmlCommentNode).Comment
                    if x.StartsWith "<!--" then
                        x.["<!--".Length .. x.Length - "-->".Length - 1]
                    else
                        x
                    |> Comment
                    |> Some
                | x -> failwithf "%A" x
            )
        f

    let ofHtmlNode (node:HtmlNode) =
        if node.NodeType = HtmlNodeType.Document then
            node.ChildNodes |> Seq.cast
        else
            Seq.singleton node
        |> ofHtmlNodes

    let toHtmlNode node =
        let d = HtmlDocument()
        d.LoadHtml (sprintNode node)
        d.DocumentNode.FirstChild

    let ofXmlNodes =
        let att (node:XmlNode) =
            node.Attributes
            |> Seq.cast<XmlAttribute>
            |> Seq.map (fun x -> x.Name, x.Value ) |> List.ofSeq
        let rec f (xs:XmlNode seq) =
            xs |> Seq.choose (fun node ->
                match node.NodeType with
                | XmlNodeType.Element ->
                    let xs = node.ChildNodes |> Seq.cast<_> |> f
                    Some <| Node(node.Name, att node, List.ofSeq xs )
                | XmlNodeType.Text ->
                    // if Parser.ParHtmlNode.isEmptyInnerText node then
                    //     None
                    // else
                        let x = node :?> XmlText
                        Some <| Text x.Value
                | XmlNodeType.Comment ->
                    // TODO: комментарии всегда отображаются как есть, `XmlWriter.WriteComment` обрамляет их в <!--...-->
                    let x = node :?> XmlComment
                    Some <| Comment x.Value
                | x -> failwithf "%A" x
            )
        f

    let ofXmlNode (node:XmlNode) =
        if node.NodeType = XmlNodeType.Document then
            node.ChildNodes |> Seq.cast
        else
            Seq.singleton node
        |> ofXmlNodes

    /// https://developer.mozilla.org/en-US/docs/Archive/Web/Writing_JavaScript_for_HTML
    let htmlToXhtml htmlContent =
        // let htmlContent = System.IO.File.ReadAllText @"E:\All2\Projects\HFParser\Test\Test\bin\Debug\net461\picturesusermarkydaysaid805012Ladybug-Mothers-Day.html"
        let d = HtmlDocument.loadHtml htmlContent
        let htmlNode =
            HtmlNode.selectSingle "html" d.DocumentNode
            |> Option.defaultWith (fun () -> failwithf "not found <html>")
        htmlNode.SetAttributeValue("xmlns", "http://www.w3.org/1999/xhtml") |> ignore
        ofHtmlNode htmlNode
        |> Seq.exactlyOne
        |> sprintNodeXDT
