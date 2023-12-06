namespace FsharpMyExtension

type 'a ArrayArray = 'a [] []

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ArrayArray =
    let columnIsEmpty columnIndex isEmpty (xss: 'a ArrayArray) =
        xss
        |> Array.forall (fun xs ->
            isEmpty xs.[columnIndex]
        )

    let trimLeft isEmpty (xss: 'a ArrayArray) =
        let columnIsEmpty columnIndex =
            columnIsEmpty columnIndex isEmpty xss

        let width = xss.[0].Length

        let leftMost =
            let rec apply x =
                if x < width && columnIsEmpty x then
                    apply (x + 1)
                else
                    x
            apply 0


        let diff = width - leftMost

        xss
        |> Array.map (fun xs ->
            Array.init diff (fun i ->
                xs.[leftMost + i]
            )
        )
