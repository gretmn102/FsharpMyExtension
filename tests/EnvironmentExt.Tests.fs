module FsharpMyExtension.EnvironmentExt.Tests
open Fuchu

open FsharpMyExtension.EnvironmentExt

let dotenv = ".env"

[<Tests>]
let ``Environment.tryGetEnvironmentVariable`` =
    testList "Environment.tryGetEnvironmentVariable" [
        testCase "from local env" <| fun () ->
            let act =
                System.IO.File.WriteAllText(dotenv, "PATH=foo")
                Environment.MutableLocalEnvironments.reload() |> ignore
                Environment.tryGetEnvironmentVariable "PATH"
            let exp =
                Some "foo"

            Assert.Equal("", exp, act)

        // testCase "from global env" <| fun () ->
        //     let act =
        //         System.IO.File.WriteAllText(dotenv, "")
        //         Environment.MutableLocalEnvironments.reload() |> ignore
        //         Environment.tryGetEnvironmentVariable "PATH"
        //     let exp =
        //         Some "foo"

        //     Assert.NotEqual("", exp, act) // todo: fix error
    ]
