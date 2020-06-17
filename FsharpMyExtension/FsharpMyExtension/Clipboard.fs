module Clipboard

let getText() = System.Windows.Forms.Clipboard.GetText()
let setText = System.Windows.Forms.Clipboard.SetText
let setTextRaw x =
    sprintf "%A" x
    |> System.Windows.Forms.Clipboard.SetText
let getSet x = getText() |> x |> setText
let getSetRaw x = getText() |> x |> setTextRaw
