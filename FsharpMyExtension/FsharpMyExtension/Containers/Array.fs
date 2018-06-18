[<RequireQualifiedAccess>]
module FsharpMyExtension.Array

let transpose xss = 
    
    let h = Array.length xss
    let w = Array.head xss |> Array.length
    let yss = Array.init w (fun _ -> Array.zeroCreate h)
    
    // for i = 0 to Array2D.length1 xss - 1 do
    //     for j = 0 to Array2D.length2 xss - 1 do
    //         yss.[i].[j] <- xss.[i,j]
    xss |> Array.iteri (fun i -> Array.iteri(fun j x -> yss.[j].[i] <- x))

    // Array2D.iteriP (fun i j x -> yss.[i].[j] <- x) xss
    yss