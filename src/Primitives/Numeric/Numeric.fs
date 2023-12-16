module FsharpMyExtension.Primitives.Numeric.Numeric


//CustomEquality;
// [<CustomComparison>]
// type 'a Rational when 'a : equality and 'a : (static member ( * ) :  'a *  'a ->  'a) =
//     | R of 'a * 'a
//     static member inline Reduce x y =
//         if y = LanguagePrimitives.GenericZero then
//             raise <| System.DivideByZeroException()
//         else
//             let d = gcd x y
//             R((x / d), (y / d))
//     static member inline (+) ((R(x,y)), (R(x',y'))) =
//         Rational<'a>.Reduce (x*y'+x'*y) (y * y')
//     static member inline (-) ((R(x,y)), (R(x',y'))) =
//         Rational<'a>.Reduce (x*y' - x'*y) (y*y')
//     static member inline (*) ((R(x,y)), (R(x',y'))) =
//         Rational<'a>.Reduce (x*x') (y*y')
//     static member inline (/) ((R(x,y)), (R(x',y'))) =
//         Rational<'a>.Reduce (x*y') (y*x')
//     static member op_LessThan ((R(x:'a,y:'a)), (R(x':'a,y':'a))) =
//         printfn "Eval"
//         x * y' <  x' * y
//     // // static member inline op_Equality ((R(x,y)), (R(x',y'))) =
//     // //     x = x' && y = y'
//     // // static member inline op_GreaterThan ((R(x,y)), (R(x',y'))) =
//     // //     x * y' >  x' * y
//     // static member internal compare(n,nn) =
//     //     if Rational<'a>.op_LessThan(n,nn) then -1
//     //     elif n = nn then 0 else 1
//     // interface System.IComparable with
//     //     member this.CompareTo(obj:obj) =
//     //         match obj with
//     //         | :? Rational<'a> as that -> Rational<'a>.compare(this,that)
//     //         | _ -> invalidArg "obj" "the objects are not comparable"

//     // override this.Equals(obj) =
//     //     match obj with
//     //     | :? Rational<'a> as that -> Rational<'a>.op_Equality(this, that)
//     //     | _ -> false
//     static member inline Neg (R(x,y)) = R(-x, y) // (~-)
//     static member inline Sign (R(x,y)) = R(sign' x, y)
// //     (x:%y) + (x':%y')   =  reduce (x*y' + x'*y) (y*y')
// //     (x:%y) - (x':%y')   =  reduce (x*y' - x'*y) (y*y')
// //     (x:%y) * (x':%y')   =  reduce (x * x') (y * y')


module Num =
    let inline gcd x y =
        let rec gcd' a b =
            if b = LanguagePrimitives.GenericZero then a
            else gcd' b (a % b)
        gcd' (abs x) (abs y)
                    // gcd x y         =  gcd' (abs x) (abs y)
                    //        where gcd' a 0  =  a
                    //              gcd' a b  =  gcd' b (a `rem` b)
    let inline sign' (x: ^a) : ^a =
        if x > LanguagePrimitives.GenericZero then LanguagePrimitives.GenericOne
        elif x < LanguagePrimitives.GenericZero then -LanguagePrimitives.GenericOne
        else LanguagePrimitives.GenericZero

[<CustomEquality;CustomComparison>]
type Rational =
    | R of int * int
    static member Reduce x y =
        if y = 0 then
            raise <| System.DivideByZeroException()
        else
            let d = Num.gcd x y
            R((x / d), (y / d))
    static member inline (+) ((R(x,y)), (R(x', y'))) =
        Rational.Reduce (x*y'+x'*y) (y * y')
    static member inline (-) ((R(x,y)), (R(x',y'))) =
        Rational.Reduce (x*y' - x'*y) (y*y')
    static member inline (*) ((R(x,y)), (R(x',y'))) =
        Rational.Reduce (x*x') (y*y')
    static member inline (/) ((R(x,y)), (R(x',y'))) =
        Rational.Reduce (x*y') (y*x')
    // static member op_LessThan ((R(x,y)), (R(x',y'))) =
    //     printfn "op_LessThan"
    //     x * y' <  x' * y
    // // // static member inline op_Equality ((R(x,y)), (R(x',y'))) =
    // // //     x = x' && y = y'
    // // static member op_GreaterThan ((R(x,y)), (R(x',y'))) =
    // //     printfn "Eval"
    // //     x * y' >  x' * y
    static member op_Equality ((R(x,y)), (R(x',y'))) = x = x' && y = y'
    static member internal compare((R(x,y) as n), (R(x',y') as nn)) =
        if x * y' <  x' * y then -1
        elif n = nn then 0 else 1
    interface System.IComparable with
        member this.CompareTo(obj:obj) =
            match obj with
            | :? Rational as that -> Rational.compare(this,that)
            | _ -> invalidArg "obj" "the objects are not comparable"

    override this.Equals(obj) =
        match obj with
        | :? Rational as that -> Rational.op_Equality(this, that)
        | _ -> false
    override this.GetHashCode () = this.GetHashCode ()
    static member inline Neg (R(x,y)) = R(-x, y) // (~-)
    static member inline Sign (R(x,y)) = R(sign x, y)
    static member Zero = R(0, 1)
    static member One = R(1, 1)
//     (x:%y) + (x':%y')   =  reduce (x*y' + x'*y) (y*y')
//     (x:%y) - (x':%y')   =  reduce (x*y' - x'*y) (y*y')
//     (x:%y) * (x':%y')   =  reduce (x * x') (y * y')

// #nowarn "77"
// [<CustomEquality;CustomComparison>]
// type RationalG< ^a when ^a : (static member (+) : ^a -> ^a -> ^a)
//                 and ^a : (static member (-) : ^a -> ^a -> ^a)
//                 and ^a : (static member (*) : ^a -> ^a -> ^a)
//                 and ^a : (static member (/) : ^a -> ^a -> ^a)
//                 and ^a : (static member (%) : ^a -> ^a -> ^a)
//                 and ^a : (static member op_LessThan : ^a -> ^a -> bool)
//                 and ^a : (static member Equal : ^a -> ^a -> bool)
//                 and ^a : (static member And : ^a -> ^a -> bool)
//                 and ^a : (static member Abs : ^a -> ^a)
//                 and ^a : (static member (~-) : ^a -> ^a)
//                 and ^a : (static member Zero: ^a)
//                 and ^a : (static member One: ^a)
//                 and ^a : comparison > =
//     | R of ^a * ^a
//     static member inline Reduce (x: ^a) (y: ^a) : RationalG< ^a> =
//         if y = LanguagePrimitives.GenericZero then
//             raise <| System.DivideByZeroException()
//         else
//             let d = Num.gcd x y
//             let (/) a b : ^a = (^a : (static member (/) : ^a -> ^a -> ^a) (a, b))

//             R((x / d), (y / d))
//     static member inline (+) ((R(x,y)) : RationalG< ^a>, (R(x', y'))) =
//         let (+) a b : ^a = (^a : (static member (+) : ^a -> ^a -> ^a) (a, b))
//         let (*) a b : ^a = (^a : (static member (*) : ^a -> ^a -> ^a) (a, b))
//         RationalG< ^a>.Reduce (x*y'+x'*y) (y * y')
//     // static member inline (-) ((R(x,y)), (R(x',y'))) =
//     //     RationalG< ^a>.Reduce (x*y' - x'*y) (y*y')
//     static member inline (-) ((R(x,y)) : RationalG< ^a>, (R(x', y'))) =
//         let (-) a b : ^a = (^a : (static member (-) : ^a -> ^a -> ^a) (a, b))
//         let (*) a b : ^a = (^a : (static member (*) : ^a -> ^a -> ^a) (a, b))
//         RationalG<'a>.Reduce (x*y' - x'*y) (y*y')
//     // static member inline (*) ((R(x,y)), (R(x',y'))) =
//     //     RationalG<'a>.Reduce (x*x') (y*y')
//     // static member inline (/) ((R(x,y)), (R(x',y'))) =
//     //     RationalG<'a>.Reduce (x*y') (y*x')
//     // // static member op_LessThan ((R(x,y)), (R(x',y'))) =
//     // //     printfn "op_LessThan"
//     // //     x * y' <  x' * y
//     // // // // static member inline op_Equality ((R(x,y)), (R(x',y'))) =
//     // // // //     x = x' && y = y'
//     // // // static member op_GreaterThan ((R(x,y)), (R(x',y'))) =
//     // // //     printfn "Eval"
//     // // //     x * y' >  x' * y
//     // static member op_Equality ((R(x,y)), (R(x',y'))) = x = x' && y = y'
//     static member inline compare((R(x,y)), (R(x',y'))) =
//         //let (<) x y = (^a : (static member op_LessThan : ^a -> ^a -> bool) (x, y))
//         let (*) a b : ^a = (^a : (static member (*) : ^a -> ^a -> ^a) (a, b))
//         // let (=) a b : ^a = (^a : (static member Equal : ^a -> ^a -> bool) (a, b))
//         // let (&&) a b : ^a = (^a : (static member And : ^a -> ^a -> bool) (a, b))
//         if x * y' <  x' * y then -1
//         elif (x = y) && (x' = y') then 0 else 1
//     interface System.IComparable< ^a> with
//         member this.CompareTo(obj: ^a) =
//             RationalG<'a>.compare(this,obj)
//             // match obj with
//             // | :? RationalG<'a> as that -> RationalG<'a>.compare(this,that)
//             // | _ -> invalidArg "obj" "the objects are not comparable"

//     // override this.Equals(obj) =
//     //     match obj with
//     //     | :? RationalG<'a> as that -> RationalG<'a>.op_Equality(this, that)
//     //     | _ -> false
//     // override this.GetHashCode () = this.GetHashCode ()
//     static member inline Neg (R(x,y) : RationalG< ^a>) = R(-x, y) : RationalG< ^a> // (~-)
//     // static member inline Sign (R(x,y) : RationalG< ^a> ) =
//     //     let sign = Num.sign' x
//     //     R(x * sign, y) : RationalG< ^a>
//     //static member inline Sign (R(x:'b,y:'b)) = R(x * (((Num.sign' : 'b -> 'b) x) : 'b), y)
//     static member inline Zero = R(LanguagePrimitives.GenericZero< ^a>, LanguagePrimitives.GenericOne< ^a>)
//     static member inline One = R(LanguagePrimitives.GenericOne< ^a>, LanguagePrimitives.GenericOne< ^a>)
// //     (x:%y) + (x':%y')   =  reduce (x*y' + x'*y) (y*y')
// //     (x:%y) - (x':%y')   =  reduce (x*y' - x'*y) (y*y')
// //     (x:%y) * (x':%y')   =  reduce (x * x') (y * y')


// [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
// [<RequireQualifiedAccess>]
// module Rational =
//     let numerator = function R(x,_) -> x
//     let denumerator = function R(_,y) -> y

//     let inline reduce x y = Rational.Reduce x y

//     let inline (%^) x y = reduce (x * sign y) (abs y)

//     // assert
//     //     let x = (1 %^ 2)
//     //     let y = (1 %^ 1)
//     //     x < y
//     // assert
//     //     let x = (2 %^ 2)
//     //     let y = (1 %^ 1)
//     //     x = y
//     // assert
//     //     let x = (1 %^ 2)
//     //     let y = (1 %^ 1)
//     //     y > x

//     let toDouble (R(x,y)) = double x / double y

//     let inline ofNum x =
//         let denum x =
//             let dec =
//                 let rec f x = function
//                     | 0 -> x
//                     | i -> f (x * 10) (i-1)
//                 f 1
//             let x = string x
//             match x.IndexOf "." with
//             | -1 -> 0 %^ 1
//             | i ->
//                 let accur = 9
//                 let f n = if n < accur then n else accur
//                 let i' = i + 1
//                 x.[i'.. i + f (x.Length - i')] |> fun x -> int x %^ dec x.Length
//         int x |> fun y ->
//         if y < 0 then y %^ 1 - (x |> denum)
//         else y %^ 1 + (x |> denum)

//     assert
//         let x = 14.
//         x |> ofNum |> toDouble = x
//     assert
//         let x = -14.34
//         x |> ofNum |> toDouble = x
//     assert
//         let x = 0.3434
//         x |> ofNum |> toDouble = x
//     // ofNum 85.05 * ofNum 0.04
//     // assert
//     //     ofFloat 28.5 = 28 %^ 1 + 5 %^ 10

//     //     ofFloat 28 |> toDouble
//     //     28. + 0.5
//     //     (5*28) %^ 10 |> toDouble
//     // string 2.04233f
//     //x - 28.5

[<CustomEquality;CustomComparison>]
type RatBig =
    {
        numerator: bigint
        denominator: bigint
    }
    //static member op_Equality ((R(x,y)), (R(x',y'))) = x = x' && y = y'
    static member op_Equality (x, y) = x.numerator = y.numerator && x.denominator = y.denominator
    static member internal compare(x, y) =
        let x, y, x', y' = x.numerator, x.denominator, y.numerator, y.denominator
        if x * y' <  x' * y then -1
        elif x = y then 0 else 1
    interface System.IComparable with
        member this.CompareTo(obj:obj) =
            match obj with
            | :? RatBig as that -> RatBig.compare(this,that)
            | _ -> invalidArg "obj" "the objects are not comparable"
    override this.Equals(obj) =
        match obj with
        | :? RatBig as that -> RatBig.op_Equality(this, that)
        | _ -> false
    override this.GetHashCode () = this.GetHashCode ()
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module RatBig =
    open System.Numerics

    let rec gcd a (b: BigInteger) =
      if b = BigInteger.Zero then a else
        gcd b (a % b)
    let lcm a b =
      (a * b) / (gcd a b)

    let mkRational p q =
      let p, q =
        if q = BigInteger.Zero then raise(System.DivideByZeroException())
        let g = gcd q p in
        p/g, q/g

      let p, q =
        if q > BigInteger.Zero then p, q else -p, -q

      { numerator = p; denominator = q }

    let intToRational (p:int) = mkRational (BigInteger(p)) BigInteger.One
    let ZeroRational = mkRational BigInteger.Zero BigInteger.One
    let OneRational = mkRational BigInteger.One BigInteger.One

    let AddRational m n =
      let d = gcd m.denominator n.denominator
      let m' = m.denominator / d
      let n' = n.denominator / d
      mkRational (m.numerator * n' + n.numerator * m') (m.denominator * n')

    let NegRational m =
      mkRational (-m.numerator) m.denominator

    let MulRational m n =
      mkRational (m.numerator * n.numerator) (m.denominator * n.denominator)

    let DivRational m n =
      mkRational (m.numerator * n.denominator) (m.denominator * n.numerator)

    let AbsRational m =
      mkRational (abs m.numerator) m.denominator

    let RationalToString m =
      if m.denominator = BigInteger.One then m.numerator.ToString() else sprintf "(%A/%A)" m.numerator m.denominator

    let GcdRational m n = mkRational (gcd m.numerator n.numerator) (lcm m.denominator n.denominator)

    let GetNumerator p = int p.numerator
    let GetDenominator p = int p.denominator
    let toDouble x = double x.numerator / double x.denominator
    assert
        AddRational (mkRational 40I 1I) (mkRational 1I 2I) |> toDouble = 40.5
    let inline ofNum x =
        let denum x =
            let dec =
                let rec f x i =
                    if i = LanguagePrimitives.GenericZero then x
                    else
                        f (x * 10I) (i - LanguagePrimitives.GenericOne)
                f LanguagePrimitives.GenericOne
            let x = string x
            let i = x.IndexOf "."
            if i = -1 then
                mkRational (bigint.Parse x) 1I
            else
                // let accur = 9
                // let f n = if n < accur then n else accur
                // let i' = i + 1
                // x.[i'.. i + f (x.Length - i')]
                x.[i + 1 .. x.Length - 1]
                |> fun x ->
                    if x = "" then mkRational 0I 1I
                    else
                        mkRational (bigint.Parse x) (dec x.Length)
                |> fun y ->
                    AddRational
                        (mkRational (bigint.Parse x.[0..i - 1]) 1I)
                        (if x.[0] = '-' then NegRational y else y)
        denum x
    assert
        let test x = x = (ofNum (string x) |> toDouble)
        List.forall id
            [ test 40.231234
              test 45.
              test 1.5 ]

    let SignRational p =
      if p.numerator < BigInteger.Zero then -1 else
      if p.numerator > BigInteger.Zero then 1 else 0

type RatBig with
    static member (+) (x, y) = RatBig.AddRational x y
    static member (*) (x, y) = RatBig.MulRational x y
    static member (/) (x, y) = RatBig.DivRational x y
    static member (-) (m, n) =
        let d = RatBig.gcd m.denominator n.denominator
        let m' = m.denominator / d
        let n' = n.denominator / d
        RatBig.mkRational (m.numerator * n' - n.numerator * m') (m.denominator * n')
    static member (~-) x = RatBig.NegRational x
    static member Sign x = RatBig.SignRational
    static member Zero = RatBig.ZeroRational
    static member One = RatBig.OneRational


type PercentType = RatBig
module private Tool =
    let max = RatBig.mkRational 100I 1I
    let min = RatBig.mkRational 0I 1I
    let plane x =
        if x > max then max
        elif x < min then min
        else x
// [<StructuredFormatDisplayAttribute("({Val})")>]
// [<Struct>]
// type PercentType(x:Numeric.Rational) =
//     member __.Val = Tool.plane x
//     // new (x) =
//     //     let s = 10

//     //     { Val = Tool.plane x }
//     //     then
//     //         PercentType <| PercentType.Plane
//     // end
//     // static member Max = Tool.max |> PercentType
//     // static member Min = Tool.min |> PercentType
//     // static member Plane (x:PercentType) = Tool.plane x.Val |> PercentType
//     static member (+) ((x:PercentType),y:PercentType) =
//         x.Val + y.Val |> Tool.plane |> PercentType
//     static member (-) ((x:PercentType),y:PercentType) = x.Val - y.Val |> Tool.plane |> PercentType
//     static member (*) ((x:PercentType),y:PercentType) = x.Val * y.Val |> Tool.plane |> PercentType
//     static member (/) ((x:PercentType),y:PercentType) = x.Val / y.Val |> Tool.plane |> PercentType


// type PercentType = Numeric.Rational
// [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
// [<RequireQualifiedAccess>]
// module PercentType =
//     let max = Tool.max : PercentType
//     let min = Tool.min : PercentType
//     let plane (x:PercentType) = Tool.plane x : PercentType
//     let inline ofNum x = Numeric.Rational.ofNum x |> plane
//     //let inline change (x:PercentType) count = x + Numeric.Rational.ofNum count |> plane
//     let inline change (x:PercentType) (count:PercentType) = x + count |> plane
//     /// процент от имеющегося.
//     /// Например, `x = 30`, `count = 0.5` -> `15`
//     /// `x = 30`, `count = 1.5` -> `45`
//     let inline changeof (x:PercentType) (count:PercentType) = x * count |> plane


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module PercentType =
    let max = Tool.max : PercentType
    let min = Tool.min : PercentType
    let plane (x:PercentType) = Tool.plane x : PercentType
    let inline ofNum x = RatBig.ofNum x |> plane
    //let inline change (x:PercentType) count = x + Numeric.Rational.ofNum count |> plane
    let inline change (x:PercentType) (count:PercentType) = RatBig.AddRational x count |> plane
    /// процент от имеющегося.
    /// Например, `x = 30`, `count = 0.5` -> `15`
    /// `x = 30`, `count = 1.5` -> `45`
    let inline changeof (x:PercentType) (count:PercentType) = x * count |> plane

