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

    let rowExists rowIndex (columnFrom, columnTo) predicate (xss: _ ArrayArray) =
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

    let rowForall rowIndex columnRange predicate xss =
        not <| rowExists rowIndex columnRange (not << predicate) xss

    let columnExists columnIndex (rowFrom, rowTo) predicate (xss: _ ArrayArray) =
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

    let columnForall columnIndex rowRange predicate (xss: _ ArrayArray) =
        not <| columnExists columnIndex rowRange (not << predicate) xss

    let crop (rowFrom, rowTo) (columnFrom, columnTo) (xss: _ ArrayArray) : _ ArrayArray =
        let height = columnTo - columnFrom
        Array.init height (fun i ->
            xss[columnFrom + i][rowFrom..rowTo - 1]
        )

    let trimLeft isEmpty (xss: 'a ArrayArray) =
        let width = getWidth xss
        let height = getHeight xss

        let columnIsEmpty columnIndex =
            columnForall columnIndex (0, height) isEmpty xss

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
