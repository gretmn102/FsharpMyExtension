module FsharpMyExtension.Collections.Set

let tryFirst (s: _ Set) =
    if Set.isEmpty s then
        None
    else
        Some (Seq.head s)
