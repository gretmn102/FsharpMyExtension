[<RequireQualifiedAccessAttribute>]
module FsharpMyExtension.Regex

/// возвращает группу вхождений, кроме первой
let matchOther pattern =
    let r = System.Text.RegularExpressions.Regex(pattern)
    r.Match
    >> fun x ->
        if x.Success then
            x.Groups |> Seq.cast<System.Text.RegularExpressions.Group>
            |> Seq.tail
            |> Seq.map (fun x -> x.Value)
            |> Some
        else None