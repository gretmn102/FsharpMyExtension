module FsharpMyExtension.Int32
let tryParse x =
    match System.Int32.TryParse x with
    | true, x -> Some x
    | false, _ -> None

let parseCharAsDigit (c:char) =
    if System.Char.IsNumber c then
        int c - int '0'
    else
        failwithf "expected decimal digit but %A" c
assert
    List.map parseCharAsDigit ['0'..'9'] = [0..9]

let getLength (n: int32) =
    let rec f acc n =
        let n = n / 10
        if n > 0 then
            f (acc + 1) n
        else
            acc
    f 1 n
