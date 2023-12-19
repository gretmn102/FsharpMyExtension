[<RequireQualifiedAccessAttribute>]
module FsharpMyExtension.Serialization.Serializers.Regex

let create (pattern: string) =
    System.Text.RegularExpressions.Regex(pattern)

let getMatches input (r: System.Text.RegularExpressions.Regex) =
    r.Matches input
    |> Seq.cast<System.Text.RegularExpressions.Match>
    |> Seq.map (fun x ->
        x.Groups
        |> Seq.cast<System.Text.RegularExpressions.Group>
        |> Seq.map (fun x -> x.Value)
    )

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
