[<RequireQualifiedAccess>]
module FsharpMyExtension.UInt64Ext.UInt64

let tryParse (str: string) =
    match System.UInt64.TryParse str with
    | false, _ -> None
    | true, n -> Some n
