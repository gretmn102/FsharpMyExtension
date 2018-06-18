module Unpoint
open FsharpMyExtension.FSharpExt
//#if INTERACTIVE
//#load "Library1.fs"
//#endif
// module ByteUnderstand =
//     open FsharpMyExtension.FSharpExt
//     open FsharpMyExtension
//     open FsharpMyExtension.List
    
//     let x = 10
//     let s = System.SByte.MaxValue
//     let toBits (x:byte) =
//         let capacity = 8
//         let s = System.Convert.ToString(x, 2) |> Array.ofSeq
//         let xs = Array.create capacity '0'
//         Array.blit s 0 xs (capacity - s.Length) s.Length
//         xs |> System.String.Concat
    
//     let n = 10uy
//     let before = toBits n
//     let after = n <<< 1 |> toBits
//     (n ^^^ (1uy <<< 0)) |> toBits |> printfn "%s"

//     let inline check reg bit = (reg >>> bit) &&& LanguagePrimitives.GenericOne
//     assert
//         let n = 14uy
//         toBits n = (List.init 8 (check n >> string) |> List.rev |> String.concat "")
//     let inline bitIsSet reg bit = check reg bit = LanguagePrimitives.GenericOne
//         //(reg &&& (LanguagePrimitives.GenericOne <<< bit )) // <> LanguagePrimitives.GenericZero
        

//     (*if BitIsSet(cod[znak], 0) OFF_a; else ON_a;
//     if BitIsSet(cod[znak], 1) OFF_b; else ON_b;
//     if BitIsSet(cod[znak], 2) OFF_c; else ON_c;
//     if BitIsSet(cod[znak], 3) OFF_d; else ON_d;
//     if BitIsSet(cod[znak], 4) OFF_e; else ON_e;
//     if BitIsSet(cod[znak], 5) OFF_f; else ON_f;
//     if BitIsSet(cod[znak], 6) OFF_g; else ON_g;*)
//     let cod = [|64uy; 121uy; 36uy; 48uy; 25uy; 18uy; 2uy; 120uy; 0uy; 16uy; 8uy; 3uy; 70uy; 33uy; 6uy; 14uy; 127uy|]
//     cod |> Array.map toBits
//     cod |> Array.map (fun x -> bitIsSet x 2)
//     //Seq.unfold (fun st -> )
//     let next = (+) '\001'
//     let xs = Seq.unfold (fun st -> Some(st, next st)) 'A' |> Seq.mapi (uncurry id)
    
//     let f (i, c) =
//         let interp = function true -> "-" | _ -> "+"
//         cod |> Seq.map (fun x -> Seq.take 7 xs |> Seq.map (mapFst (bitIsSet x >> interp) >> curry (sprintf "%s%c")) ) |> Seq.map (String.concat " ")
        
//         List.init (Array.length cod) (bitIsSet (Array.get cod i)) |> List.map (interp >> flip (sprintf "%s%c") c)
//     xs |> Seq.map f |> Seq.take 7 |> List.ofSeq |> List.trans |> List.map (String.concat " ")

module Unpoint =
    open FsharpMyExtension
    open FsharpMyExtension.Show

    type Var = string
    type Expr = 
        | Lambda of Var * Expr
        | App of Expr * Expr
        | Var of Var
    let rec printExprLine = function
        | Lambda(arg, body) -> sprintf "Lambda(%A, %s)" arg <| printExprLine body
        | App(x, y) -> sprintf "App(%s, %s)" <| printExprLine x <| printExprLine y
        | Var s -> sprintf "Var %A" s
    let rec fold fn (st:'State) = function
        | Var x -> fn st x
        //| h::t -> fold <| fn <| fn st h <| t
        | Lambda(e, v) -> fold fn st v
        | App(e1, e2) -> fold fn <| fold fn st e1 <| e2
        
    let rec map fn x = 
        match fn x with
        | Lambda(v, e) -> Lambda(v, map fn e)
        | App(e, e2) -> App(map fn e, map fn e2)
        | Var _ as x -> x


    let print = 
        let interp = function
            | "op_Addition" -> "+"
            | "op_Subtraction" -> "-"
            | "op_Multiply" -> "*"
            | "Identity" -> "id"
            | x -> x
        let rec f paren = function
        | Lambda(x, body) -> 
            showParen paren (showString "fun " << showString x << showString " -> " << f false body)
        | App(App(Var op1, x), App(App(Var op2, y), z)) ->
            let op1, op2 = interp op1, interp op2
            let res =
                match op1, op2 with
                | (a, b) when a = b -> f false x << showString op1 << f false y << showString op2 << f false z
                | "+", "*" -> f false x << showString op1 << f false y << showString op2 << f false z
                | "*", "+" -> f false x << showString op1 << showParen true (f false y << showString op2 << f false z)
                | op1, op2 -> showString op1 << f false x << showParen true (showString op2 << f false y << f false z)
            //let res e1 = showParen e1 (f false x << showString op1 << f false y) << showString op2 << f false z
            showParen paren res
        | App(App (Var op, x), y) ->
            let op = interp op
            let res = 
                match op with
                | "*" | "+" -> f false x << showString op << f false y
                | _ -> showString op << f false x << f false y
            showParen paren res
//        | App(App(x, y), z) ->
//            let fn = function Var x -> interp x | e -> sprintf "(%s)" <| f e
//            sprintf "%s %s %s" <| fn x <| fn y <| fn z
        | App(x, y) as curr ->
//            let rec f' xs  = function
//                | App(x, y) -> f' <| y::xs <| x
//                | Var _ as x -> x::xs, None
//                | x -> xs, Some x
            //assert (App(App(Var "a", Var "b"), Var "c") |> f' [] = ([Var "a"; Var "b"; Var "c"], None))
            let cond = function Var _ -> false | _ -> true
            let cond2 = function Lambda _ -> true | _ -> false
            showParen paren (f (cond2 x) x << showChar ' ' << f (cond y) y)
//            let fn = function Var x -> interp x | e -> sprintf "(%s)" <| f e
//            sprintf "%s %s" <| fn x <| fn y
//            let c, rest = f' [] curr
        | Var x -> interp x |> showString
        in f false >> show

    assert (print <| Lambda("x", Var "y") = "fun x -> y")
    assert (print <| Lambda("x", Lambda("y", Var "z")) = "fun x -> fun y -> z")
    assert (print <| App(Var "x", App(Var "y", Var "z")) = "x (y z)")
    assert (print <| App(Var "x", Lambda("y", Var "z")) = "x (fun y -> z)")
    assert (print <| App(Var "x", App(Lambda("y", Var "z"), Var "w" )) = "x ((fun y -> z) w)")
    assert (print <| App(Lambda("x", Var "y"), Var "z") = "(fun x -> y) z")
    assert (print <| App(App(Var "x", Var "y"), Var "z") = "x y z")

    //print <| App(App(Lambda("x", App(Var "x", Var "z")), Var "x'"), Lambda("x", App(Var "x", Var "z")))
    let rec pars = function
        | Quotations.Patterns.Lambda(v, l) -> Lambda(v.Name, pars l)
        | Quotations.Patterns.Call(_, fn, args) ->
            if List.isEmpty args then Var fn.Name
            else List.fold(fun st x -> App(st, pars x)) <| Var fn.Name <| args
        | Quotations.Patterns.Value(o, _) -> Var <| string o
        | Quotations.Patterns.Var v -> Var v.Name
        | Quotations.Patterns.Application(x, y) -> App(pars x, pars y)
        | x -> failwithf "unknown %A" x
    //pars <@ fun f n -> f * (2 + 3) * (fun x -> x * 2) (f + n) @> |> print
    let contain elem =
        let rec fn curr =
            match curr with
            | App(x, y) -> if elem = curr then true else if fn x then true else fn y
            | Lambda(x, body) -> 
                match elem with
                | Var v -> if v = x then false else fn body
                | _ -> fn body
            | _ -> curr = elem
        in fn
    assert
        contain <| Var "z" <| App(Var "y", Lambda("x", App(Var "x", Var "z")))
    assert
        contain <| Var "x" <| Lambda("x",App(Var "z",Var "x"))
    
    let fn = 
        let add x y = App(y, Var x)
        let rec fn curr = function
            | App (App (op, Var x), Var y) when curr = x && curr = y ->
                App (App (Var "s", fn curr op), Var "Identity") |> add curr
            | App (App (f, Var x),App (g, Var y)) when curr = x && curr = y ->
                App (App (Var "s", fn curr f), fn curr g) |> add curr
            | App (App (e, Var x ), y) when x = curr ->
                App(App (Var "flip", fn curr e), fn curr y) |> add curr
            | App (f, App(g, Var x)) when x = curr ->
                App (App (Var "b", fn curr f), fn curr g) |> add curr
            | Lambda (x, body) ->
                let rec f = function
                    | App(body, Var y) when x = y -> 
                        if contain <| Var x <| body then f (fn x <| App(body, Var y))
                        else body
                    | body -> f <| fn x body
                f body
            | App ((App (f, g) as f'), Var x) when x = curr ->
                if contain <| Var curr <| f || contain <| Var curr <| g then
                    App(fn curr f', Var x)
                else App(fn curr f, fn curr g)
            //| App(Var x, Var y) when x = y -> failwith "(fun x -> x x)"
            | App(f, g) -> App(fn curr f, fn curr g)
            | x -> x
        fn ""
    let sample = <@ fun x y z -> x + (y - z) + (z - 1) @> |> pars

    assert
        let expected = fun x y z -> x + (y - z) + (z - 1)
        let actual =
            //flip (b flip (b (b s) (flip (b flip (b (b flip) (b (b (b b)) (b (b (b (+))) (flip (b b (b b (+))) (-)))))) (flip (-) 1)))) id
            flip (flip << (((<<) s) << (flip (flip << (((<<) flip) << (((<<) <| (<<) (<<)) << (((<<) <| (<<) (+)) << (flip <| (<<) (<<) ((<<) << (+)) <| (-)))))) <| flip (-) 1))) id
        let test x y z = expected x y z = actual x y z
        test 30 13 -234
    
    
//    sample |> fn "" |> fn "z" |> fn "z" |> fn "z" |> fn "z" |> fn "z" |> fn "y" |> fn "y" |> fn "y" |> fn "y" |> fn "y" |> fn "y" |> fn "y" |> fn "y" |> fn "x" |> fn "x" |> fn "x" |> fn "x" |> fn "x" |> fn "x" |> fn "x" |> fn "x" |> fn "x" |> fn "x" |> fn "x" |> fn "x" |> print

    assert
        // <@ fun x y z -> x + (y - z) + (z - 1) @> |> pars
        let input = Lambda("x", Lambda("y", Lambda("z", App(App(Var "op_Addition", App(App(Var "op_Addition", Var "x"), App(App(Var "op_Subtraction", Var "y"), Var "z"))), App(App(Var "op_Subtraction", Var "z"), Var "1")))))
        let expected = App(App(Var "flip", App(App(Var "b", Var "flip"), App(App(Var "b", App(Var "b", Var "s")), App(App(Var "flip", App(App(Var "b", Var "flip"), App(App(Var "b", App(Var "b", Var "flip")), App(App(Var "b", App(Var "b", App(Var "b", Var "b"))), App(App(Var "b", App(Var "b", App(Var "b", Var "op_Addition"))), App(App(Var "flip", App(App(Var "b", Var "b"), App(App(Var "b", Var "b"), Var "op_Addition"))), Var "op_Subtraction")))))), App(App(Var "flip", Var "op_Subtraction"), Var "1"))))), Var "Identity")
//        input |> fn "" |> print
//        let sd = Seq.unfold(fun st -> match fn "" st with c when c = st -> None | c -> Some(st, c))
//         
//        sample |> sd |> Seq.last |> fn "" |> print
//        sample |> sd |> Seq.last |> print
//        sample |> sd |> Seq.last = Lambda("x",App(expected, Var "x"))
//        fn "" <| (pars <@ fun f x -> f x @>) |> fn ""
        let actual = fn input
        actual = expected