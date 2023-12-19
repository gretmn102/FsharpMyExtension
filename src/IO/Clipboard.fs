module FsharpMyExtension.IO.Clipboard

let getText() = TextCopy.ClipboardService.GetText()
let setText = TextCopy.ClipboardService.SetText
let setTextRaw x =
    sprintf "%A" x
    |> setText
let getSet x = getText() |> x |> setText
let getSetRaw x = getText() |> x |> setTextRaw
