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

/// Determines the number of digits in the target number.
let getLength n =
    let rec loop acc i =
        if i > 0 then
            loop (acc + 1) (i / 10)
        else
            acc
    loop 0 n
