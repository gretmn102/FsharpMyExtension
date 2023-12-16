[<RequireQualifiedAccess>]
module FsharpMyExtension.Collections.Dictionary
open System.Collections.Generic

let tryGetValue key (dic: #IDictionary<_, _>) =
    match dic.TryGetValue key with
    | true, value -> Some value
    | false, _ -> None
