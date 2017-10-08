namespace FsharpMyExtension.HtmlNode
open HtmlAgilityPack

module HtmlNode =
    open FsharpMyExtension.Option

    let createNode html = HtmlNode.CreateNode html
    let getAttVal name (node:HtmlNode) = node.GetAttributeValue(name, null)
    assert
        [ HtmlNode.CreateNode "<div att='val'></div>" |> getAttVal "att" = "val"
          HtmlNode.CreateNode "<div att='val'></div>" |> getAttVal "att2" |> isNull ] |> List.forall id
    
    let tryGetAttVal name (node:HtmlNode) = node.GetAttributeValue(name, null) |> Option.ofNull
    assert
        [ HtmlNode.CreateNode "<div att='val'></div>" |> tryGetAttVal "att" = Some "val"
          HtmlNode.CreateNode "<div att='val'></div>" |> tryGetAttVal "att2" = None ] |> List.forall id
    let name (node:HtmlNode) = node.Name
    let innerText (node:HtmlNode) = node.InnerText
    let nodeType (node:HtmlNode) = node.NodeType

    let ofString s = HtmlDocument() |> fun d -> d.LoadHtml s |> fun () -> d.DocumentNode
    let selectSingle s (node:HtmlNode) = node.SelectSingleNode s |> Option.ofNull
    let selectNodes s (node:HtmlNode) = node.SelectNodes s |> Option.ofNull |> Option.map (Seq.cast<HtmlNode>)
