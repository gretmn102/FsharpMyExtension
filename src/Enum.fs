[<RequireQualifiedAccess>]
module FsharpMyExtension.Enum

let inline is<'Enum when 'Enum : (static member (|||): 'Enum * 'Enum -> 'Enum) and 'Enum : equality>
    (value: 'Enum)
    (current: 'Enum) =

    (value ||| current) = current

/// Equals `Enum.HasFlag`
let inline contains<'Enum when 'Enum : (static member (&&&): 'Enum * 'Enum -> 'Enum) and 'Enum : equality>
    (value: 'Enum)
    (current: 'Enum) =

    (value &&& current) = current
