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

    type TrimOption =
        | Top = 0b01
        | Right = 0b10
        | Bottom = 0b100
        | Left = 0b1000

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module TrimOption =
        let all = TrimOption.Top ||| TrimOption.Right ||| TrimOption.Bottom ||| TrimOption.Left

    let trimBounds (trimOption: TrimOption) isEmpty (xss: _ ArrayArray) =
        let width = getWidth xss
        let height = getHeight xss

        let columnIsEmpty columnIndex rowRange =
            columnForall columnIndex rowRange isEmpty xss

        let rowIsEmpty rowIndex columnRange =
            rowForall rowIndex columnRange isEmpty xss

        let rowFrom =
            if Enum.contains TrimOption.Top trimOption then
                let rec loop rowIndex =
                    if rowIndex < height && rowIsEmpty rowIndex (0, width) then
                        loop (rowIndex + 1)
                    else
                        rowIndex
                loop 0
            else
                0

        let rowTo =
            if Enum.contains TrimOption.Bottom trimOption then
                let rec loop rowIndex =
                    if rowIndex > rowFrom && rowIsEmpty rowIndex (0, width) then
                        loop (rowIndex - 1)
                    else
                        rowIndex + 1
                loop (height - 1)
            else
                height

        if not (rowFrom < rowTo) then
            None
        else
            let columnFrom =
                if Enum.contains TrimOption.Left trimOption then
                    let rec loop columnIndex =
                        if columnIndex < width && columnIsEmpty columnIndex (rowFrom, rowTo) then
                            loop (columnIndex + 1)
                        else
                            columnIndex
                    loop 0
                else
                    0

            let columnTo =
                if Enum.contains TrimOption.Right trimOption then
                    let rec loop columnIndex =
                        if columnIndex > columnFrom && columnIsEmpty columnIndex (rowFrom, rowTo) then
                            loop (columnIndex - 1)
                        else
                            columnIndex + 1
                    loop (width - 1)
                else
                    width

            if not (columnFrom < columnTo) then
                None
            else
                Some ((columnFrom, columnTo), (rowFrom, rowTo))

    let trim trimOption isEmpty (xss: _ ArrayArray) : option<_ ArrayArray> =
        trimBounds trimOption isEmpty xss
        |> Option.map (fun (rowRange, columnRange) ->
            crop rowRange columnRange xss
        )
