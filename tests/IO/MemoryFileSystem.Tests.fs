module FsharpMyExtension.IO.MemoryFileSystem.Tests
open Fuchu

[<Tests>]
let ``IO.MemoryFileSystem.create`` =
    testList "IO.MemoryFileSystem.create" [
        testCase "base case" <| fun () ->
            Assert.Equal(
              "",
              Directory (
                  Map ["discord", Directory (
                      Map ["users", Directory (
                          Map ["lumi.md", File "Парень по имени Lumi"])])]),
              create ["discord"; "users"; "lumi.md"] "Парень по имени Lumi"
            )
    ]
