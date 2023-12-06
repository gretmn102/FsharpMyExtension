namespace FsharpMyExtension

type 'a ArrayArray = 'a [] []

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ArrayArray =
    let getWidth (xss: _ ArrayArray) =
        match Array.tryHead xss with
        | Some xs -> Array.length xs
        | None -> 0

    let getHeight (xss: _ ArrayArray) =
        Array.length xss

    let horizontalExists rowIndex (columnFrom, columnTo) predicate (xss: _ ArrayArray) =
        let row = xss[rowIndex]
        let rec loop columnFrom =
            if columnFrom < columnTo then
                if predicate row[columnFrom] then
                    true
                else
                    loop (columnFrom + 1)
            else
                false
        loop columnFrom

    let horizontalForall rowIndex columnRange predicate xss =
        not <| horizontalExists rowIndex columnRange (not << predicate) xss

    let verticalExists columnIndex (rowFrom, rowTo) predicate (xss: _ ArrayArray) =
        let rec loop rowFrom =
            if rowFrom < rowTo then
                let row = xss[rowFrom]
                if predicate row[columnIndex] then
                    true
                else
                    loop (rowFrom + 1)
            else
                false
        loop rowFrom

    let verticalForall columnIndex rowRange predicate (xss: _ ArrayArray) =
        not <| verticalExists columnIndex rowRange (not << predicate) xss

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
