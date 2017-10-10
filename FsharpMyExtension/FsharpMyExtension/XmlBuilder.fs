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
        use output2 = new XmlTextWriter(sw, Formatting=Formatting.Indented)
        use writer = XmlWriter.Create(output2)
        let rec f = function
            | Node(tag, atts, body) ->
                writer.WriteStartElement tag
                atts |> List.iter writer.WriteAttributeString
                List.iter f body
                writer.WriteEndElement()
            | Text s -> writer.WriteString s
        f x
        writer.Close()
    let sprintNode (x:Node) =
        let output = System.Text.StringBuilder()
        sprint output x
        output.ToString()
    let sprintNodeDT (x:Node) =
        let sb = System.Text.StringBuilder()
        sb.AppendLine "<!DOCTYPE html>" |> ignore
        sprint sb x
        sb.ToString()