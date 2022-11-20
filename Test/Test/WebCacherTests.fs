module WebCacherTests
open Fuchu
open FsharpMyExtension

[<Tests>]
let getsTests =
    testList "getsTests" [
        testCase "base" <| fun () ->
            let get urls webCacher =
                WebCacher.gets id (fun x -> x) urls webCacher

            let res, webCacher =
                let urls =
                    [
                        "https://translate.google.com"
                        "https://media.tenor.com/yVKQAhFuGZQAAAAC/cat-bite.gif"
                    ]
                get urls WebCacher.empty

            let res2, webCacher =
                let urls =
                    [
                        "https://translate.google.com"
                        "https://media.tenor.com/yVKQAhFuGZQAAAAC/cat-bite.gif"
                        "https://media.tenor.com/iFjm7dyo_-MAAAAd/cat-bite.gif"
                    ]
                get urls webCacher

            // todo
            // let exp = todoExp
            // let act = todoAct

            // Assert.Equal("", exp, act)
            ()
    ]
