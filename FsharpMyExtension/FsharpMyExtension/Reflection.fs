module FsharpMyExtension.Reflection

module Reflection =
    /// **Description**
    ///
    /// In module declared:
    /// ```fsharp
    /// type internal IMarker = interface end
    /// let t = typeof<IMarker>.DeclaringType
    /// ```
    /// after that, send in this function `%ModuleName%.t`
    ///
    /// **Output Type**
    ///  * `System.Type -> values:string [] * functions:string []`
    ///  * `values` - function without arguments
    ///
    /// **Exceptions**
    /// `System.ArgumentException` if arg is not module
    let getFnNamesFromModule =
        let xs = set["ToString"; "Equals"; "GetHashCode"; "GetType"]
        fun (t:System.Type) ->
        if Reflection.FSharpType.IsModule t then
            let prop = t.GetProperties() |> Array.map (fun x -> x.Name)
            let fns =
                let xs = prop |> Array.fold (fun st x -> Set.add (sprintf "get_%s" x) st) xs
                t.GetMethods () |> Array.filter (fun x -> t.IsSpecialName |> not && not (Set.contains x.Name xs ))
                |> Array.map (fun x -> x.Name)
            prop, fns
        else raise <| System.ArgumentException "this is not module"

    /// Enumeration values of union. All values must be empty type.
    let unionEnum<'T> =
        let xs = Reflection.FSharpType.GetUnionCases typeof<'T>
        xs |> Array.map (fun x ->
            if x.GetFields() |> Array.isEmpty |> not then
                failwithf "%A must be empty" x.Name
            else Reflection.FSharpValue.MakeUnion(x,[||]) |> unbox<'T>)
    let recordInitS<'T> =
        let xs = Reflection.FSharpType.GetRecordFields typeof<'T>
        xs |> Array.map (fun x -> sprintf "    %s = failwith \"Not implemented\"" x.Name)
        |> String.concat "\n" |> sprintf "{\n%s\n}"

    let inline recordInit<'T> f =
        let t = typedefof< 'T>
        let xs = Reflection.FSharpType.GetRecordFields t
        let ys = Array.init (Array.length xs) f
        Reflection.FSharpValue.MakeRecord(t, ys)
        |> unbox< 'T>

    /// Init all unions in Map with GenericZero<'Typ>
    let inline initUnionMap< 'Union, ^Typ when ^Typ : (static member Zero : ^Typ) and 'Union : comparison > =
        let z = LanguagePrimitives.GenericZero< 'Typ>
        unionEnum<'Union>
        |> Array.map (fun x -> x, z)
        |> Map.ofArray

    let inline initUnionMapV (v: ^Typ) =
        unionEnum< ^Union>
        |> Array.map (fun x -> x, v)
        |> Map.ofArray

    let recordEq cond (x:'a) (y:'a) =
        let t = x.GetType()
        if not <| Reflection.FSharpType.IsRecord t then
            failwith "arg is not record"
        let fs =
            Reflection.FSharpType.GetRecordFields t
            |> Array.map (fun x -> x.Name)
        let xs = Reflection.FSharpValue.GetRecordFields x
        let ys = Reflection.FSharpValue.GetRecordFields y
        Array.map3 (fun x y z -> x, (y, z)) fs xs ys
        |> Array.filter cond

    let printProperties main =
        let t = main.GetType()
        t.GetMembers()
        |> Array.filter (fun x -> x.MemberType = System.Reflection.MemberTypes.Property)
        |> Array.map (fun x ->
            let y = x :?> System.Reflection.PropertyInfo
            try
                let y = y.GetMethod.Invoke(main, [||])
                string y
            with _ -> "?"
            |> comma x.Name
            )