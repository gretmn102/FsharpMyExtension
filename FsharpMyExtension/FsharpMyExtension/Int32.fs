module FsharpMyExtension.Int32
let tryParse x = 
    match System.Int32.TryParse x with
    | true, x -> Some x
    | false, _ -> None
