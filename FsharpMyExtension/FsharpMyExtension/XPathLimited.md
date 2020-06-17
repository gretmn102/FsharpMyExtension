Модуль предназначен для проверки, что данный XML-узел обладает таким-то именем, атрибутами и, возможно, имеет текстовый подузел с определенным значением.

```fsharp
open FsharpMyExtension.XPathLimited
open HtmlAgilityPack
let htmlContent =
    "<tag attrname1=\"attrVal1\" attrname2=\"attrVal2\" attrname3=\"attrVal3\">text in node</tag>"
let doc = HtmlDocument()
doc.LoadHtml htmlContent
let xpath =
    "tag[@attrname1][@attrname2='attrVal2'][@*='attrVal3'][text()='text in node']"
doc.DocumentNode.FirstChild
|> HtmlNode.isMatch xpath // -> true
```

XPath используется урезанные, т.е. он воспринимает лишь что-то простое, вроде:
* `a[@atr][text()='a1']`
* `tag[@attrname1][@attrname2='attrVal2'][@*='attrVal3'][text()='text in node']`

Никаких `/` не должно быть.

Ах да, стоит отметить, как обрабатывается `text()`: в этой реализации подузел должен быть **только** одним, а в реализации `HtmlAgilityPack.HtmlNode.SelectSingleNode` их может быть несколько.
Например:
```fsharp
open FsharpMyExtension.XPathLimited
open HtmlAgilityPack
open FsharpMyExtension.Either
let htmlContent =
    "<tag attrname1=\"attrVal1\" attrname2=\"attrVal2\" attrname3=\"attrVal3\">text in node<b>some bold</b>text in node2<b>again</b>text in node3</tag>"
let doc = HtmlDocument()
doc.LoadHtml htmlContent
let node = doc.DocumentNode.FirstChild

let xpath =
    "tag[text()='text in node']"
node
|> HtmlNode.isMatch xpath // -> false

Parser.run xpath
|> Either.getOrDef' (failwithf "%A")
|> ShowReq.showSelf // self::node()[name()='tag'][text()='text in node']
|> node.SelectSingleNode |> (not << isNull) // -> true

// А то и вовсе:
"self::node()[name()='tag'][text()='text in node3'][text()='text in node']"
|> node.SelectSingleNode |> (not << isNull) // -> true
```

# Истоки
Вручную прописывать — сущее наказание, например:
```fsharp
let exampleHtmlContent =
    "<tag attrname1=\"attrVal1\" attrname2=\"attrVal2\" attrname3=\"attrVal3\">text in node</tag>"

open HtmlAgilityPack
let exampleHtml = HtmlAgilityPack.HtmlNode.CreateNode exampleHtmlContent

let isMatch (node:HtmlAgilityPack.HtmlNode) =
    let attributes = node.Attributes
    let is (name:string, value) =
        match attributes.[name] with
        | null -> false
        | x -> x.Value = value
    let hasValue v =
        attributes |> Seq.exists (fun x -> x.Value = v)
    let hasTextNode txt =
        let xs = node.ChildNodes
        if xs.Count = 1 then
            match xs.[0] with
            | :? HtmlAgilityPack.HtmlTextNode as x -> x.InnerText = txt
            | _ -> false
        else false
    node.Name = "tag"
    && attributes.Contains "attrname1"
    && is ("attrname2", "attrVal2")
    && hasValue "attrVal3"
    && hasTextNode "text in node"
isMatch exampleHtml // -> true
```
Страшно, правда? К счастью, существует XPath, который и подкинул идею.

<!-- TODO: Поскольку от XPath требуется всего ничего, то запрос вообще можно преобразовать в нечто подобное:
```
tag attrname1 attrname2=attrVal2 *=attrVal3>text in node
```
Потом как-нибудь воплощу. -->