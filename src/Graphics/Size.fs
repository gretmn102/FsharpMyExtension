namespace FsharpMyExtension.Grahics

type Size =
    {
        Width: int
        Height: int
    }
    static member (*) ((this: Size), (scale: float)) =
        let apply (side: int) =
            int (float side * scale)
        {
            Width = apply this.Width
            Height = apply this.Height
        }
    static member (*) ((scale: float), (this: Size)) =
        this * scale

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Size =
    let create w h = {
        Width = w
        Height = h
    }
