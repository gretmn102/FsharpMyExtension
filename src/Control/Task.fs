module FsharpMyExtension.Control.Task
open System.Threading.Tasks

let continueWith (next: 'a -> 'b) (task: Task<'a>) : Task<'b> =
    task.ContinueWith (fun (task: Task<'a>) -> next task.Result)

let await (task: Task<'a>) =
    task.ConfigureAwait(false).GetAwaiter().GetResult()

let awaiti (task: Task) =
    task.ConfigureAwait(false).GetAwaiter().GetResult()

let runSync (millisecondsDelay: int) (tasks: (unit -> Task<'a>) list) =
    let rec loop acc = function
        | getTask::getTasks ->
            let res =
                // // available on newer FSharp.Core
                // task {
                //     let! res = getTask ()
                //     do! Task.Delay millisecondsDelay
                //     return res
                // }
                // |> await
                let res = getTask () |> await
                Task.Delay(millisecondsDelay) |> awaiti
                res
            loop (res::acc) getTasks
        | [] ->
            List.rev acc
    loop [] tasks
