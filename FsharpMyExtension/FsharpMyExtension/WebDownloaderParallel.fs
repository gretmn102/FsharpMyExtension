module FsharpMyExtension.WebDownloaderParallel

open System
open System.IO
open FsharpMyExtension
open FsharpMyExtension.List
open FsharpMyExtension.Either
System.Net.ServicePointManager.DefaultConnectionLimit <- Int32.MaxValue

assert
    let charsToStream xs =
        let x = xs |> Array.ofSeq
        let m = new MemoryStream(x.Length)
        let w = new StreamWriter(m)
        w.Write(x)
        w.Flush()
        m.Position <- 0L
        m
    let xs = [|'a'..'e'|]
    
    let offset = 1
    let arr = Array.create (xs.Length + offset) 0uy
    let m = charsToStream xs
    m.Read(arr, offset, xs.Length) |> ignore
    m.Close()
    Text.Encoding.UTF8.GetBytes xs = arr.[offset..arr.Length - 1]

assert
    let partsCount = 5
    let xs = ['A'..'z'] |> List.collect (List.replicate 1)
    
    let inline erase source target (off, next) = 
        //Seq.iteri (fun i x -> Array.set out (int off + i) x)
        let on f x y = f x, f y
        let off, next = on int off next
        Array.blit source 0 target off (next - off + 1)

    let parts = List.splitIntoRange partsCount xs.Length
    let length = Seq.length xs
    let arr = 
        Array.ofList xs |> fun xs ->
        parts |> List.map (fun (i,j) -> xs.[int i.. int j])

    let notPar () =
        let out = Array.create length (char 0)
        List.zip arr parts |> Seq.iter (fun (xs, y) -> erase xs out y)
        out
    let par () =
        let out = Array.create length (char 0)
        List.zip arr parts |> Seq.map (fun (xs, y) -> async { erase xs out y })
        |> Async.Parallel |> Async.RunSynchronously |> ignore
        out
    let arr = Array.ofList xs
    par () = arr && arr = notPar()

/// max 2046Mb
/// предполагается, что `Accept-Ranges` доступен.
let download (parts:int) createHttp length =
    let rec down output (start:int32, stop) =
        // `start` — индекс, с которого начинается запись (включительно).
        // `stop` — конечный индекс (включительно).
        let rec f () = 
            let (request:System.Net.HttpWebRequest) = createHttp()
            request.AddRange(start, stop)
            request.Timeout <- 5000
            try
                // let request = System.Net.HttpWebRequest.CreateHttp("http://joyreactor.cc")
                // request
                request.GetResponse() //:?> System.Net.HttpWebResponse
            with e ->
                // System.Net.WebException
                printfn "%A" e
                printfn "try again %d, %d" start stop
                // printfn "try again"
                f ()
        use resp = f ()
        // resp.Headers.ToString() |> printfn "%s"
        use inp = resp.GetResponseStream()
        let partially () = 
            let rec read curr =
                let buffLength = stop - curr + 1
                try
                    let len = inp.Read(output, curr, buffLength)
                    if len > 0 then read (curr + len)
                    else None
                with e ->
                    printfn "%A" e
                    Some curr
            read start
        match partially () with
        | None -> ()
        | Some curr ->
            System.Threading.Thread.Sleep 1000
            // TODO: будем бесконечно гонять при отключенном интернете?
            down output (curr, stop)            
        // printfn "end %d, %d" x y
    let arr = Array.create length 0uy
    List.splitIntoRange parts length
    |> Seq.map (fun x -> async { down arr x })
    |> Async.Parallel |> Async.RunSynchronously |> ignore
    arr


let defineLengthAndRanges url =
    let hwRq = System.Net.HttpWebRequest.Create(url:string)
    hwRq.Method <- "HEAD"
    use resp = hwRq.GetResponse()
    match resp.Headers.Get "Accept-Ranges" with
    | null -> None
    | _ -> resp.ContentLength |> Some

/// max 2046Mb
let downloadDefineLength parts url =
    let request = fun () -> System.Net.HttpWebRequest.CreateHttp(url:string)
    defineLengthAndRanges url
    |> Option.map (int >> download parts request)
let test2 () =
    let url = "http://joyreactor.cc/images/icon_en.png"
    downloadDefineLength 4 url
    let length = defineLengthAndRanges url |> Option.get
    let request = fun () -> System.Net.HttpWebRequest.CreateHttp(url:string)
    download 4 request (int length)
    ()
    
let tryDownloadSimple (url:string) = 
    // let url = "https://www.youtube.com/watch?v=AB2_6r6yjHM&list=PLkU8l1ITH2CUnb3vPQm6x1M5YncpWqJuI"
    let hwRq = System.Net.HttpWebRequest.Create(url)
    try
        use resp = hwRq.GetResponse()
        use out = resp.GetResponseStream()
        use m = new MemoryStream()
        out.CopyTo m
        m.ToArray() |> Right
    with e -> Left e
let test () =
    // 44.36Mb
    let url = "http://img1.joyreactor.cc/pics/post/гифки-Кошачьи-живность-3901997.gif"
    let url = "http://img1.joyreactor.cc/pics/post/commitstrip-Комиксы-4633351.jpeg"
    // System.Net.WebUtility.UrlDecode url
    /// ~409.49Mb
    let url409 =
        "https://r4---sn-4gxb5u-qo3z.googlevideo.com/videoplayback?&aitags=133,134,135,136,137,160,242,243,244,247,248,278&alr=yes&c=WEB&clen=429385340&cpn=XWFkEG2KDPsAEZEk&cver=2.20180703&dur=1654.652&ei=ulZCW8vdC4bB7gTW4oHwCQ&expire=1531095834&fexp=23709359&fvip=6&gir=yes&id=o-AH2pHKv7ivuZvWDb0yLhzLjZWeHonv2Z1z9DekD7gNL5&initcwndbps=957500&ip=46.98.186.5&ipbits=0&itag=248&keepalive=yes&key=yt6&lmt=1524468151265449&mime=video/webm&mm=31,29&mn=sn-4gxb5u-qo3z,sn-3c27sn7k&ms=au,rdu&mv=m&nh=,IgpwcjAxLmticDAzKgkxMjcuMC4wLjE&pcm2cms=yes&pl=16&rbuf=0&requiressl=yes&rn=0&signature=9E011675C811BBA3EBE06509CEB93BD2A1629759.9EF56FC3771971872F961523E1CE464ACDB02A8E&source=youtube&sparams=aitags,clen,dur,ei,gir,id,initcwndbps,ip,ipbits,itag,keepalive,lmt,mime,mm,mn,ms,mv,nh,pcm2cms,pl,requiressl,source,expire"
    // System.Int32.MaxValue
    // let count = Option.get (def url) |> int
    // split (int64 46518244) 4
    let expect = tryDownloadSimple url |> Either.get
    // 00:13:36.299 — 409Mb
    // 00:00:10.835 — 44.36Mb
    // 00:00:10.798
    let actual = downloadDefineLength 4 url |> Option.get
    // System.Text.Encoding.UTF8.GetString expect
    // System.Text.Encoding.UTF8.GetString actual
    // 00:03:18.260 — 409Mb
    // 00:00:11.113 — 44.36Mb
    // 00:00:10.023
    // Seq.zip expect actual
    // |> Seq.indexed
    // |> Seq.find (snd >> curry (<>))

    expect = actual


// let rec g i = 
//     if i < 10 then 
//         let rec f j : int =
//             try
//                 if j < 10 then f (j + 1)
//                 else
//                     failwith ""
//             with e ->
//                 g (i + 1)
//         f 0
//     else failwith ""
// g 0