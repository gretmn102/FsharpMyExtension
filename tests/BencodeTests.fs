module BencodeTests
open FsharpMyExtension
open Fuchu

[<Tests>]
let serializationTests =
    testList "serializationTests" [
        testCase "serialize and deserialize" <| fun () ->
            let input =
                Bencode.dic [
                    ("announce", Bencode.str "http://my.site/ann?uk=123abc")
                    ("comment", Bencode.str "https://mysite.net/forum/viewtopic.php?t=123")
                    ("created by", Bencode.str "uTorrent/3.4.9")
                    ("creation date", Bencode.int 1519660763)
                    ("encoding", Bencode.str "UTF-8")
                    ("info",
                        Bencode.dic [
                            ("files",
                                Bencode.List [
                                    Bencode.dic [
                                        ("length", Bencode.int 32)
                                        ("path", Bencode.list [Bencode.str "Reward.ini"])
                                    ]
                                    Bencode.dic [
                                        ("length", Bencode.int 434)
                                        ("path", Bencode.list [Bencode.str "Game.exe"])
                                    ]

                                    Bencode.dic [
                                        ("length", Bencode.int 235)
                                        ("path",
                                            Bencode.list
                                                [Bencode.str "Audio"; Bencode.str "BGM"; Bencode.str "sound.mid"])
                                    ]
                                    Bencode.dic [
                                        ("length", Bencode.int 234)
                                        ("path",
                                            Bencode.list [Bencode.str "Audio"; Bencode.str "SE"; Bencode.str "sound2.mp3"])
                                    ]
                                    Bencode.dic [
                                        ("length", Bencode.int 54)
                                        ("path",
                                            Bencode.list
                                                [Bencode.str "Audio"; Bencode.str "BGM"; Bencode.str "sound3.mid"])
                                    ]
                                    Bencode.dic [
                                        ("length", Bencode.int 54236)
                                        ("path",
                                            Bencode.list
                                                [Bencode.str "Audio"; Bencode.str "SE"; Bencode.str "sound4.mp3"])
                                    ]
                                    Bencode.dic [
                                        ("length", Bencode.int 23646)
                                        ("path",
                                            Bencode.list
                                                [Bencode.str "Audio"; Bencode.str "SE"; Bencode.str "sound5.mp3"])
                                    ]
                                    Bencode.dic [
                                        ("length", Bencode.int 2344)
                                        ("path",
                                            Bencode.list
                                                [Bencode.str "Audio"; Bencode.str "SE"; Bencode.str "sound6.mp3"])
                                    ]
                                ])
                            ("name", Bencode.str "Foo Game of Bar")
                            ("piece length", Bencode.int 2097152)
                            ("pieces", Bencode.str "")
                            ("publisher", Bencode.str "mysite.net")
                            ("publisher-url", Bencode.str "https://mysite.net/forum/viewtopic.php?t=123")
                        ]
                    )
                ]

            let act =
                Bencode.serialize input
                |> Bencode.deserialize

            let exp = Ok input

            Assert.Equal("", exp, act)
    ]
