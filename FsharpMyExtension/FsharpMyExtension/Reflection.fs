namespace FsharpMyExtension.Reflection

module Reflection =
    ///**Description**
    /// In module declared:
    /// * `type internal IMarker = interface end`
    /// * `let t = typeof<IMarker>.DeclaringType`
    /// after that, send in this function `%ModuleName%.t`
    ///**Output Type**
    ///  * `System.Type -> values:string [] * functions:string []`
    ///  * `values` - function without arguments
    ///**Exceptions**
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