namespace FsharpMyExtension.HtmlAgilityPackExt
open HtmlAgilityPack

module HtmlDocument =
    let loadHtml x =
        let d = HtmlDocument()
        d.LoadHtml x
        d
    let load (path:string) =
        let d = HtmlAgilityPack.HtmlDocument()
        d.Load(path)
        d
module HtmlNode =
    let createNode html = HtmlNode.CreateNode html
    let getAttVal name (node:HtmlNode) = node.GetAttributeValue(name, null)
    assert
        [ HtmlNode.CreateNode "<div att='val'></div>" |> getAttVal "att" = "val"
          HtmlNode.CreateNode "<div att='val'></div>" |> getAttVal "att2" |> isNull ] |> List.forall id

    let tryGetAttVal name (node:HtmlNode) = node.GetAttributeValue(name, null) |> Option.ofObj
    assert
        [ HtmlNode.CreateNode "<div att='val'></div>" |> tryGetAttVal "att" = Some "val"
          HtmlNode.CreateNode "<div att='val'></div>" |> tryGetAttVal "att2" = None ] |> List.forall id
    let name (node:HtmlNode) = node.Name
    let innerText (node:HtmlNode) = node.InnerText
    let nodeType (node:HtmlNode) = node.NodeType

    let ofString x =
        let d = HtmlDocument.loadHtml x
        d.DocumentNode
    /// **Exceptions**
    ///
    /// `System.Xml.XPath.XPathException`
    let selectSingle (s:string) (node:HtmlNode) =
        node.SelectSingleNode s |> Option.ofObj

    ///**Description**
    ///
    /// If `HtmlNode.SelectNodes` is nothing found, then he return `null`.
    ///
    ///**Exceptions**
    ///
    /// `System.Xml.XPath.XPathException`
    let selectNodes (s:string) (node:HtmlNode) =
        node.SelectNodes s |> Option.ofObj |> Option.map (Seq.cast<HtmlNode>)
    assert
        let nd = HtmlNode.CreateNode "<a><b /><c /></a>"
        selectNodes "d" nd = None

    /// `System.String.IsNullOrWhiteSpace`
    let IsNullOrWhiteSpace (node:HtmlNode) =
        node.InnerText
        |> System.String.IsNullOrWhiteSpace
