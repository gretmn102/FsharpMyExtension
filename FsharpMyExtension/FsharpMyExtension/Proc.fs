module Proc
/// запускает процесс и ждет его завершения.
let startProc dataReceived progPath args = 
    let startInfo = System.Diagnostics.ProcessStartInfo()
    startInfo.FileName <- progPath
    startInfo.Arguments <- args
    
    startInfo.UseShellExecute <- false
    startInfo.RedirectStandardOutput <- true

    use proc = new System.Diagnostics.Process()
    proc.EnableRaisingEvents <- true
    
    proc.OutputDataReceived.AddHandler(
        System.Diagnostics.DataReceivedEventHandler(
            fun _ args -> dataReceived args.Data
        )
    )
    proc.StartInfo <- startInfo
    proc.Start() |> ignore
    proc.BeginOutputReadLine()
    proc.WaitForExit()
    proc.ExitCode
/// Выводит на консоль
let startProcSimple path args =
    startProc (fun e -> printfn "%s" e) path args
/// Выводит на консоль и одновременно в `string`
let startProcString path args =
    let drivenOutput = new System.Text.StringBuilder()
    startProc (fun e ->
        printfn "%s" e
        drivenOutput.AppendLine(e) |> ignore
    ) path args
    |> fun code -> code, drivenOutput.ToString()