module FsharpMyExtension.Serialization.DataFormats.Bencode.Tests
open Fuchu

open FsharpMyExtension.Serialization.DataFormats

[<Tests>]
let serializationTests =
    testList "serializationTests" [
        testCase "serialize and deserialize" <| fun () ->
            let input =
                dic [
                    ("announce", str "http://my.site/ann?uk=123abc")
                    ("comment", str "https://mysite.net/forum/viewtopic.php?t=123")
                    ("created by", str "uTorrent/3.4.9")
                    ("creation date", int 1519660763)
                    ("encoding", str "UTF-8")
                    ("info",
                        dic [
                            ("files",
                                Bencode.List [
                                    dic [
                                        ("length", int 32)
                                        ("path", list [str "Reward.ini"])
                                    ]
                                    dic [
                                        ("length", int 434)
                                        ("path", list [str "Game.exe"])
                                    ]

                                    dic [
                                        ("length", int 235)
                                        ("path",
                                            list
                                                [str "Audio"; str "BGM"; str "sound.mid"])
                                    ]
                                    dic [
                                        ("length", int 234)
                                        ("path",
                                            list [str "Audio"; str "SE"; str "sound2.mp3"])
                                    ]
                                    dic [
                                        ("length", int 54)
                                        ("path",
                                            list
                                                [str "Audio"; str "BGM"; str "sound3.mid"])
                                    ]
                                    dic [
                                        ("length", int 54236)
                                        ("path",
                                            list
                                                [str "Audio"; str "SE"; str "sound4.mp3"])
                                    ]
                                    dic [
                                        ("length", int 23646)
                                        ("path",
                                            list
                                                [str "Audio"; str "SE"; str "sound5.mp3"])
                                    ]
                                    dic [
                                        ("length", int 2344)
                                        ("path",
                                            list
                                                [str "Audio"; str "SE"; str "sound6.mp3"])
                                    ]
                                ])
                            ("name", str "Foo Game of Bar")
                            ("piece length", int 2097152)
                            ("pieces", str "")
                            ("publisher", str "mysite.net")
                            ("publisher-url", str "https://mysite.net/forum/viewtopic.php?t=123")
                        ]
                    )
                ]

            let act =
                serialize input
                |> deserialize

            let exp = Ok input

            Assert.Equal("", exp, act)
    ]
