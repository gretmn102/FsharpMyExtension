module FsharpMyExtension.Control.Task.Tests
open FsharpMyExtension
open Fuchu
open System.Threading.Tasks

open Helpers

[<Tests>]
let ``Task.runSync`` =
    testList "Task.runSync" [
        testCase "base" <| fun () ->
            let count = 10
            let tasks =
                Array.init count (fun i ->
                    fun () ->
                        use t = new Task<_>(fun () -> i)
                        t.RunSynchronously()
                        t
                )
                |> List.ofArray

            Expect.equal
                (runSync 100 tasks)
                [0..count - 1]
                ""
    ]
