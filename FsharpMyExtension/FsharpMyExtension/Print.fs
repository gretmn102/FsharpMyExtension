module Print

let reader () =
    let s = System.Text.StringBuilder()
    printfn "for the exit, put line with in end ';;'"
    
    let rec f () =
        let curr = System.Console.ReadLine()
        if curr.EndsWith(";;") then
            let x = curr.[0 .. curr.Length - 3]
            s.Append(x)
        else
            s.AppendLine curr |> ignore
            f()
    f() |> ignore
    s.ToString()

let run f = reader() |> String.collect f |> printfn "Result:\n%s"

///**Description**
/// escape '\' -> "\\" | '"' -> '\"\"'
let escape () =
    let f = function
        | '\\' -> "\\\\"
        | '"' -> "\\\""
        | x -> string x
    run f

///**Description**
/// escape '\' -> "\\" | '"' -> '\"\"' and \n -> "\\n" | \r -> ""
let escapen () =
    let f = function
        | '\\' -> "\\\\"
        | '"' -> "\\\""
        | '\n' -> "\\n"
        | '\r' -> ""
        | x -> string x
    run f

///**Description**
/// for @ string ('"' -> '""')
let escape2 () =
    let f = function
        | '"' -> "\"\""
        | x -> string x
    run f