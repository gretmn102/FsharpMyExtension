[<RequireQualifiedAccess>]
module FsharpMyExtension.Enum

let inline is<'EnumType, 'Enum when 'Enum: enum<'EnumType> and 'Enum: equality>
    (value: 'Enum)
    (current: 'Enum)
    =

    value = current

/// Equals `Enum.HasFlag`
let inline contains<'EnumType, 'Enum when 'Enum: enum<'EnumType> and 'Enum: (static member (|||): 'Enum * 'Enum -> 'Enum) and 'Enum: equality>
    (value: 'Enum)
    (current: 'Enum) =

    (value ||| current) = current
