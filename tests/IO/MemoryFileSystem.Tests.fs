module FsharpMyExtension.IO.MemoryFileSystem.Tests
open Fuchu

open Helpers

[<Tests>]
let ``IO.MemoryFileSystem.create`` =
    testList "IO.MemoryFileSystem.create" [
        testCase "base case" <| fun () ->
            Expect.equal
                (create ["discord"; "users"; "lumi.md"] "Парень по имени Lumi")
                (Entity.Directory (
                    Map ["discord", Entity.Directory (
                        Map ["users", Entity.Directory (
                            Map ["lumi.md", Entity.File "Парень по имени Lumi"])])]))
                ""
    ]

[<Tests>]
let ``IO.MemoryFileSystem.writeFile`` =
    testList "IO.MemoryFileSystem.writeFile" [
        testCase "is directory error" <| fun () ->
            Expect.equal
                (writeFile
                    ["lumi.md"] "Парень по имени Lumi"
                    (Map [
                        "lumi.md", Entity.Directory Map.empty
                    ])
                )
                (Error WriteFileError.IsDirectory)
                ""
        testCase "empty path error" <| fun () ->
            Expect.equal
                (writeFile
                    [] "Парень по имени Lumi"
                    Map.empty
                )
                (Error WriteFileError.PathFragmentsIsEmpty)
                ""
        testCase "create and write file in empty directory" <| fun () ->
            Expect.equal
                (writeFile
                    ["lumi.md"] "Парень по имени Lumi"
                    Map.empty)
                (Ok <| Map [
                    "lumi.md", Entity.File "Парень по имени Lumi"
                ])
                ""
        testCase "create and write file in not empty directory" <| fun () ->
            Expect.equal
                (writeFile
                    ["lumi.md"] "Парень по имени Lumi"
                    (Map ["somefile", Entity.File "some content"]))
                (Ok <| Map [
                    "lumi.md", Entity.File "Парень по имени Lumi"
                    "somefile", Entity.File "some content"
                ])
                ""
        testCase "rewrite one directory file" <| fun () ->
            Expect.equal
                (writeFile
                    ["lumi.md"] "Парень по имени Lumi"
                    (Map ["lumi.md", Entity.File "empty"]))
                (Ok <| Map [
                    "lumi.md", Entity.File "Парень по имени Lumi"
                ])
                ""
        testCase "rewrite one subdirectory file" <| fun () ->
            Expect.equal
                (writeFile
                    ["users"; "lumi.md"] "Парень по имени Lumi"
                    (Map [
                        "users", Entity.Directory (Map [
                            "lumi.md", Entity.File "empty"
                        ])
                    ])
                )
                (Ok <| Map [
                    "users", Entity.Directory (Map [
                        "lumi.md", Entity.File "Парень по имени Lumi"
                    ])
                ])
                ""
        testCase "create discord/users/lumi.md in discord/index.md" <| fun () ->
            Expect.equal
                (writeFile
                    ["discord"; "users"; "lumi.md"] "Парень по имени Lumi"
                    (Map [
                        "discord", Entity.Directory (Map [
                            "index.md", Entity.File ""
                        ])
                    ])
                )
                (Ok <| Map [
                    "discord", Entity.Directory (Map [
                        "index.md", Entity.File ""
                        "users", Entity.Directory (Map [
                            "lumi.md", Entity.File "Парень по имени Lumi"
                        ])
                    ])
                ])
                ""
        testCase "create discord/users/lumi.md in discord" <| fun () ->
            Expect.equal
                (writeFile
                    ["discord"; "users"; "lumi.md"] "Парень по имени Lumi"
                    (Map [
                        "discord", Entity.Directory Map.empty
                    ])
                )
                (Ok <| Map [
                    "discord", Entity.Directory (Map [
                        "users", Entity.Directory (Map [
                            "lumi.md", Entity.File "Парень по имени Lumi"
                        ])
                    ])
                ])
                ""
    ]

[<Tests>]
let ``IO.MemoryFileSystem.readFile`` =
    testList "IO.MemoryFileSystem.readFile" [
        testCase "read lumi.md" <| fun () ->
            Expect.equal
                (readFile
                    ["lumi.md"]
                    (Map [
                        "lumi.md", Entity.File "Парень по имени Lumi"
                    ])
                )
                (Ok "Парень по имени Lumi")
                ""
        testCase "read discord/users/lumi.md" <| fun () ->
            Expect.equal
                (readFile
                    ["discord"; "users"; "lumi.md"]
                    (Map [
                        "discord", Entity.Directory (Map [
                            "users", Entity.Directory (Map [
                                "lumi.md", Entity.File "Парень по имени Lumi"
                            ])
                        ])
                    ])
                )
                (Ok "Парень по имени Lumi")
                ""
        testCase "read unexist file error" <| fun () ->
            Expect.equal
                (readFile
                    ["discord"; "users"; "lumi.md"]
                    (Map [
                        "discord", Entity.Directory Map.empty
                    ])
                )
                (Error ReadFileError.FileNotFound)
                ""
        testCase "try read directory" <| fun () ->
            Expect.equal
                (readFile
                    ["discord"; "users"]
                    (Map [
                        "discord", Entity.Directory (Map [
                            "users", Entity.Directory Map.empty
                        ])
                    ])
                )
                (Error ReadFileError.IsDirectory)
                ""
        // todo: add one of the path fragments hits a file error
    ]

[<Tests>]
let ``IO.MemoryFileSystem.remove`` =
    testList "IO.MemoryFileSystem.remove" [
        testCase "remove lumi.md" <| fun () ->
            Expect.equal
                (remove
                    ["lumi.md"]
                    (Map [
                        "lumi.md", Entity.File "Парень по имени Lumi"
                    ])
                )
                (Ok (Entity.Directory Map.empty))
                ""
        testCase "remove discord/users/lumi.md" <| fun () ->
            Expect.equal
                (remove
                    ["discord"; "users"; "lumi.md"]
                    (Map [
                        "discord", Entity.Directory (Map [
                            "users", Entity.Directory (Map [
                                "lumi.md", Entity.File "Парень по имени Lumi"
                            ])
                        ])
                    ])
                )
                (Ok <| Entity.Directory (Map [
                    "discord", Entity.Directory (Map [
                        "users", Entity.Directory Map.empty
                    ])
                ]))
                ""
        testCase "remove discord/users/lumi.md with index.md" <| fun () ->
            Expect.equal
                (remove
                    ["discord"; "users"; "lumi.md"]
                    (Map [
                        "discord", Entity.Directory (Map [
                            "users", Entity.Directory (Map [
                                "index.md", Entity.File ""
                                "lumi.md", Entity.File "Парень по имени Lumi"
                            ])
                        ])
                    ])
                )
                (Ok <| Entity.Directory (Map [
                    "discord", Entity.Directory (Map [
                        "users", Entity.Directory (Map [
                            "index.md", Entity.File ""
                        ])
                    ])
                ]))
                ""
        testCase "remove unexist file error" <| fun () ->
            Expect.equal
                (remove
                    ["discord"; "users"; "lumi.md"]
                    (Map [
                        "discord", Entity.Directory Map.empty
                    ])
                )
                (Error RemoveError.EntityNotFound)
                ""
        testCase "PathFragmentsIsEmpty error" <| fun () ->
            Expect.equal
                (remove
                    []
                    (Map [
                        "discord", Entity.Directory (Map [
                            "users", Entity.Directory Map.empty
                        ])
                    ])
                )
                (Error RemoveError.PathFragmentsIsEmpty)
                ""
    ]
