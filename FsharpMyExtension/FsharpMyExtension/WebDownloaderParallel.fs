module FsharpMyExtension.WebDownloaderParallel
open FsharpMyExtension
open FsharpMyExtension.Either

let reading read offset count =
    let rec f offset count =
        if count > 0 then
            let count' = read offset count
            if count' > 0 then
                f (offset + count') (count - count')
    f offset count

let readingByRange read offset stop =
    let rec f curr =
        let buffLength = stop - curr + 1
        try
            let len = read curr buffLength
            if len > 0 then f (curr + len)
            else None
        with e ->
            Some(e, curr)
    f offset

/// max 2046Mb
let download (parts:int) reqf url =
    let rec read (resp:System.Net.WebResponse) output (start,stop) =
        use inp = resp.GetResponseStream()
        let read offset count = inp.Read(output, offset, count)
        match readingByRange read start stop with
        | None -> ()
        | Some(e, curr) ->
            printfn "%A" e
            System.Threading.Thread.Sleep 1000
            // TODO: будем бесконечно гонять при отключенном интернете?
            down output (curr, stop)
    and down output (start:int32, stop) =
        // `start` — индекс, с которого начинается запись (включительно).
        // `stop` — конечный индекс (включительно).
        let rec f () =
            let (request:System.Net.HttpWebRequest) =
                reqf (FsharpMyExtension.WebDownloader.createHttpReq url)

            request.AddRange(start, stop)
            request.Timeout <- 5000
            try
                request.GetResponse()
            with e ->
                printfn "%s" e.Message
                printfn "trying again %d, %d..." start stop
                f ()
        use resp = f ()
        read resp output (start, stop)

    let resp =
        let rec f () =
            let (request:System.Net.HttpWebRequest) =
                reqf (FsharpMyExtension.WebDownloader.createHttpReq url)

            request.Timeout <- 5000
            try
                Right (request.GetResponse())
            with
                | :? System.Net.WebException as e ->
                    if e.Status = System.Net.WebExceptionStatus.ProtocolError then
                        use resp = e.Response :?> System.Net.HttpWebResponse
                        // let tooManyRequests = (enum 429 : System.Net.HttpStatusCode)
                        // printfn "%s" x.Message
                        match resp.StatusCode with
                        | System.Net.HttpStatusCode.NotFound
                        | System.Net.HttpStatusCode.Forbidden ->
                            Left e.Message
                        | _ ->
                            printfn "%s" e.Message
                            printfn "trying again..."
                            f ()
                    else
                        printfn "%s" e.Message
                        printfn "trying again..."
                        f ()

                | e ->
                    printfn "%s" e.Message
                    printfn "trying again..."
                    f ()
        f ()
    resp
    |> Either.bind (fun resp ->
        using resp (fun resp ->
            match resp.Headers.Get "Accept-Ranges" with
            | null ->
                use out = resp.GetResponseStream()
                use m = new System.IO.MemoryStream()
                out.CopyTo m
                m.ToArray() |> Right
            | _ ->
                let length = int32 resp.ContentLength
                let buff = Array.create length 0uy
                match List.splitIntoRange parts length with
                | x::xs ->
                    let x = async { read resp buff x }
                    let xs = xs |> List.map (fun x -> async { down buff x })
                    x :: xs
                | [] -> failwith "if parts <= 0"
                |> Async.Parallel |> Async.RunSynchronously |> ignore
                Right buff
        )
    )

let internal test3 () =
    // open FsharpMyExtension.Either
    let tryDownloadSimple reqf (url:string) =
        // let url = "https://www.youtube.com/watch?v=AB2_6r6yjHM&list=PLkU8l1ITH2CUnb3vPQm6x1M5YncpWqJuI"
        let (request:System.Net.HttpWebRequest) =
            reqf (FsharpMyExtension.WebDownloader.createHttpReq url)
        try
            use resp = request.GetResponse()
            use out = resp.GetResponseStream()
            use m = new System.IO.MemoryStream()
            out.CopyTo m
            m.ToArray() |> Right
        with e -> Left e
    // 44.36Mb
    let url = "http://img1.joyreactor.cc/pics/post/гифки-Кошачьи-живность-3901997.gif"

    let url = "http://img1.joyreactor.cc/pics/post/commitstrip-Комиксы-4633351.jpeg"
    // System.Net.WebUtility.UrlDecode url
    /// ~409.49Mb
    let url =
        "https://r4---sn-4gxb5u-qo3z.googlevideo.com/videoplayback?&aitags=133,134,135,136,137,160,242,243,244,247,248,278&alr=yes&c=WEB&clen=429385340&cpn=XWFkEG2KDPsAEZEk&cver=2.20180703&dur=1654.652&ei=ulZCW8vdC4bB7gTW4oHwCQ&expire=1531095834&fexp=23709359&fvip=6&gir=yes&id=o-AH2pHKv7ivuZvWDb0yLhzLjZWeHonv2Z1z9DekD7gNL5&initcwndbps=957500&ip=46.98.186.5&ipbits=0&itag=248&keepalive=yes&key=yt6&lmt=1524468151265449&mime=video/webm&mm=31,29&mn=sn-4gxb5u-qo3z,sn-3c27sn7k&ms=au,rdu&mv=m&nh=,IgpwcjAxLmticDAzKgkxMjcuMC4wLjE&pcm2cms=yes&pl=16&rbuf=0&requiressl=yes&rn=0&signature=9E011675C811BBA3EBE06509CEB93BD2A1629759.9EF56FC3771971872F961523E1CE464ACDB02A8E&source=youtube&sparams=aitags,clen,dur,ei,gir,id,initcwndbps,ip,ipbits,itag,keepalive,lmt,mime,mm,mn,ms,mv,nh,pcm2cms,pl,requiressl,source,expire"
    // System.Int32.MaxValue
    // let count = Option.get (def url) |> int
    // split (int64 46518244) 4
    let expect = tryDownloadSimple id url
    // 00:13:36.299 — 409Mb
    // 00:00:10.835 — 44.36Mb
    // 00:00:10.798
    // open FsharpMyExtension.WebDownloaderParallel
    let actual = download 4 id url
        // |> Either.get
        // downloadDefineLength 4 url |>
    System.IO.File.WriteAllBytes("output\\some.gif", Either.get actual)
    // System.Text.Encoding.UTF8.GetString expect
    // System.Text.Encoding.UTF8.GetString actual
    // 00:03:18.260 — 409Mb
    // 00:00:11.113 — 44.36Mb
    // 00:00:10.023
    // Seq.zip expect actual
    // |> Seq.indexed
    // |> Seq.find (snd >> curry (<>))
    Either.get expect = Either.get actual
