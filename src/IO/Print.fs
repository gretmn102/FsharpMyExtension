module FsharpMyExtension.IO.Print
open FsharpMyExtension.Primitives

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

let run reader f = reader() |> String.collect f |> printfn "Result:\n%s"

///**Description**
/// `escape '\' -> "\\\\" | '"' -> "\\\""`
let escape () =
    let f = function
        | '\\' -> "\\\\"
        | '"' -> "\\\""
        | x -> string x
    run reader f

///**Description**
/// escape '\' -> "\\" | '"' -> '\"\"' and \n -> "\\n" | \r -> ""
let escapen () =
    let f = function
        | '\\' -> "\\\\"
        | '"' -> "\\\""
        | '\n' -> "\\n"
        | '\r' -> ""
        | x -> string x
    run reader f
///**Description**
/// escape '\' -> "\\" | '"' -> '\"\"' and \n -> "\\n" | \r -> ""
let escapens s =
    let f = function
        | '\\' -> "\\\\"
        | '"' -> "\\\""
        | '\n' -> "\\n"
        | '\r' -> ""
        | x -> string x
    run (fun () -> s) f


///**Description**
/// for @ string ('"' -> '""')
let escape2 () =
    let f = function
        | '"' -> "\"\""
        | x -> string x
    run reader f

let toSnippet s =
    s
    |> String.lines
    |> Array.map (String.collect (function
            | '\\' -> "\\\\"
            | '"' -> "\\\""
            | '\n' -> "\\n"
            | '\r' -> ""
            | x -> string x) >> sprintf "\"%s\"")
    |> String.concat ",\n" |> printfn "%s"

module TreadSafe =
    let mail = MailboxProcessor.Start (fun agent ->
        let rec loop () =
            async {
                let! msg = agent.Receive()
                printfn "%s" msg
                return! loop ()
            }
        loop ()
    )
    let printfn fmt = Printf.ksprintf mail.Post fmt
