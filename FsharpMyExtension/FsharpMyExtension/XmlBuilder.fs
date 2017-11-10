namespace FsharpMyExtension.XmlBuilder
type Att = string * string
type Node =
    | Text of string
    | Node of string * Att list * Node list
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
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
            | Text s -> writer.WriteString s
        f x
    let sprintNode (x:Node) =
        let output = System.Text.StringBuilder()
        sprint output x
        output.ToString()
    let sprintNodeDT (x:Node) =
        let sb = System.Text.StringBuilder()
        sb.AppendLine "<!DOCTYPE html>" |> ignore
        sprint sb x
        sb.ToString()
    open HtmlAgilityPack
    let ofHtmlNode (node:HtmlNode) =
        let att (node:HtmlNode) =
            node.Attributes |> Seq.map (fun x -> x.Name, x.Value ) |> List.ofSeq
        let rec f (xs:HtmlNode list) =
            xs |> List.choose (fun node ->
                match node.NodeType with
                | HtmlNodeType.Element ->
                    let xs = node.ChildNodes |> List.ofSeq |> f
                    Some <| Node(node.Name, att node, xs )
                | HtmlNodeType.Text ->
                    if Parser.ParHtmlNode.isEmptyInnerText node then None
                    else
                        let x = node :?> HtmlTextNode
                        Some <| Text x.Text
                | HtmlNodeType.Comment -> None
                | x -> failwithf "%A" x)
        match node.NodeType with
        | HtmlNodeType.Element ->
            Node(node.Name, att node, f (List.ofSeq node.ChildNodes))
        | HtmlNodeType.Text ->
            if Parser.ParHtmlNode.isEmptyInnerText node then Text ""
            else
                let x = node :?> HtmlTextNode
                Text x.Text
        | x -> failwithf "not support %A\\only support 'Element' or 'Text'" x
        
    let toHtmlNode node =
        let d = HtmlDocument()
        d.LoadHtml (sprintNode node)
        d.DocumentNode.FirstChild