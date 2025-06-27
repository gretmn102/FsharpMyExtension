module FsharpMyExtension.IO.MemoryFileSystem.Tests
open Fuchu

open Helpers

[<Tests>]
let ``IO.MemoryFileSystem.create`` =
    testList "IO.MemoryFileSystem.create" [
        testCase "base case" <| fun () ->
            Expect.equal
                (create ["discord"; "users"; "lumi.md"] "Парень по имени Lumi")
                (Directory (
                    Map ["discord", Directory (
                        Map ["users", Directory (
                            Map ["lumi.md", File "Парень по имени Lumi"])])]))
                ""
    ]

[<Tests>]
let ``IO.MemoryFileSystem.writeFile`` =
    testList "IO.MemoryFileSystem.writeFile" [
        testCase "is directory error" <| fun () ->
            Expect.equal
                (writeFile
                    ["lumi.md"] "Парень по имени Lumi"
                    (Directory (Map [
                        "lumi.md", Directory Map.empty
                    ]))
                )
                (Error WriteFileError.IsDirectory)
                ""
        testCase "empty path error" <| fun () ->
            Expect.equal
                (writeFile
                    [] "Парень по имени Lumi"
                    (Directory Map.empty)
                )
                (Error WriteFileError.PathFragmentsIsEmpty)
                ""
        testCase "create and write file in empty directory" <| fun () ->
            Expect.equal
                (writeFile
                    ["lumi.md"] "Парень по имени Lumi"
                    (Directory Map.empty))
                (Ok <| Directory (Map [
                    "lumi.md", File "Парень по имени Lumi"
                ]))
                ""
        testCase "create and write file in not empty directory" <| fun () ->
            Expect.equal
                (writeFile
                    ["lumi.md"] "Парень по имени Lumi"
                    (Directory (Map ["somefile", File "some content"])))
                (Ok <| Directory (Map [
                    "lumi.md", File "Парень по имени Lumi"
                    "somefile", File "some content"
                ]))
                ""
        testCase "rewrite one directory file" <| fun () ->
            Expect.equal
                (writeFile
                    ["lumi.md"] "Парень по имени Lumi"
                    (Directory (Map ["lumi.md", File "empty"])))
                (Ok <| Directory (Map [
                    "lumi.md", File "Парень по имени Lumi"
                ]))
                ""
        testCase "rewrite just file" <| fun () ->
            Expect.equal
                (writeFile
                    ["lumi.md"] "Парень по имени Lumi"
                    (File "empty"))
                (Error WriteFileError.FileSystemStartAsFile)
                ""
        testCase "rewrite one subdirectory file" <| fun () ->
            Expect.equal
                (writeFile
                    ["users"; "lumi.md"] "Парень по имени Lumi"
                    (Directory (Map [
                        "users", Directory (Map [
                            "lumi.md", File "empty"
                        ])
                    ]))
                )
                (Ok <| Directory (Map [
                    "users", Directory (Map [
                        "lumi.md", File "Парень по имени Lumi"
                    ])
                ]))
                ""
        testCase "create discord/users/lumi.md in discord/index.md" <| fun () ->
            Expect.equal
                (writeFile
                    ["discord"; "users"; "lumi.md"] "Парень по имени Lumi"
                    (Directory (Map [
                        "discord", Directory (Map [
                            "index.md", File ""
                        ])
                    ]))
                )
                (Ok <| Directory (Map [
                    "discord", Directory (Map [
                        "index.md", File ""
                        "users", Directory (Map [
                            "lumi.md", File "Парень по имени Lumi"
                        ])
                    ])
                ]))
                ""
        testCase "create discord/users/lumi.md in discord" <| fun () ->
            Expect.equal
                (writeFile
                    ["discord"; "users"; "lumi.md"] "Парень по имени Lumi"
                    (Directory (Map [
                        "discord", Directory Map.empty
                    ]))
                )
                (Ok <| Directory (Map [
                    "discord", Directory (Map [
                        "users", Directory (Map [
                            "lumi.md", File "Парень по имени Lumi"
                        ])
                    ])
                ]))
                ""
    ]
