module FsharpMyExtension.WebDownloader
open System.Net
open FsharpMyExtension.Either

let cookies = CookieContainer()
let defaultEncoding = System.Text.Encoding.UTF8
ServicePointManager.DefaultConnectionLimit <- System.Int32.MaxValue

type Url = string
type Content = string
type Res = Url * Either<System.Exception, HttpStatusCode * Content>
module Decryption =
    let gzip (encoding:System.Text.Encoding) (receiveStream:System.IO.Stream) =
        try
            use zl = new System.IO.Compression.GZipStream(receiveStream,System.IO.Compression.CompressionMode.Decompress)
            use r = new System.IO.StreamReader(zl, encoding)
            let str = r.ReadToEnd()
            Right str
        with e -> Left e
let createHttpReq (url:string) =
    // let url = "https://google.com"
    let uri = System.Uri url
    let req = System.Net.WebRequest.CreateHttp(uri)
    req.UserAgent <- "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:34.0) Gecko/20100101 Firefox/34.0"
    match System.IO.Path.GetExtension url with
    | ".webp" ->
        req.Accept <- "video/webm,video/ogg,video/*;q=0.9,application/ogg;q=0.7,audio/*;q=0.6,*/*;q=0.5"
    | ".mp4" ->
        req.Accept <- "video/mp4,video/ogg,video/*;q=0.9,application/ogg;q=0.7,audio/*;q=0.6,*/*;q=0.5"
    | ".gif" ->
        req.Accept <- "image/gif"
    | _ ->
        req.Accept <- "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
        req.Headers.Add (System.Net.HttpRequestHeader.AcceptLanguage, "ru-RU,ru;q=0.8,en-US;q=0.5,en;q=0.3")
    req.Referer <- sprintf "%s://%s" uri.Scheme uri.Host
    req.CookieContainer <- cookies
    req.Timeout <- 2500
    req

open FsharpMyExtension.Net

let defineEncoding contentType =
    match contentType with
    | null -> defaultEncoding
    | contentType ->
        let res = ContentType.Parser.start contentType
        match res with
        | Right x ->
            match x.Parameter with
            | Some (ContentType.Charset enc) -> enc
            | None -> defaultEncoding
            | Some _ -> defaultEncoding
        | Left x ->
            printfn "ContentTypeParser error:\n%A" x
            defaultEncoding

/// Если выбьет: дескать, не удалось создать защищенный канал SSL/TLS, — то:
/// ```fsharp
/// System.Net.ServicePointManager.SecurityProtocol <- System.Net.SecurityProtocolType.Tls12
/// ```
let tryGet reqf (url:string) =
    let req : HttpWebRequest = reqf (createHttpReq url)
    req.Headers.Add (System.Net.HttpRequestHeader.AcceptEncoding, "gzip")
    try
        use resp = req.GetResponse() :?> HttpWebResponse

        let encoding = defineEncoding resp.ContentType
        match resp.Headers.["Content-Encoding"] with
        | "gzip" ->
            try
                use receiveStream = resp.GetResponseStream()
                use zl = new System.IO.Compression.GZipStream(receiveStream,System.IO.Compression.CompressionMode.Decompress)
                use r = new System.IO.StreamReader(zl, encoding)
                let str = r.ReadToEnd()
                url, Right(resp.StatusCode, str) : Res
            with e -> url, Left e
        | _ -> // `null` возьмет?
            use stream = resp.GetResponseStream()
            use reader = new System.IO.StreamReader(stream, encoding)
            url, Right(resp.StatusCode, reader.ReadToEnd()) : Res
    with e -> url, Left e

// assert
//     let c = CookieContainer()
//     let uri = Uri("https://www.google.com")
//     c.SetCookies(uri, "PHPSESSID=1")
//     c.GetCookieHeader(uri)
//     let s f = lock c f
//     let xs = Seq.init 10000 (fun i -> async { c.SetCookies(uri, sprintf "PHPSESSID=%d" i) })
//     Async.Parallel xs |> Async.RunSynchronously |> ignore
//     c.GetCookieHeader uri


//     let monitor = Object()
//     let a = ref 4

//     printfn "1) a = %i" !a

//     let t1 = System.Threading.Thread (fun () ->
//         printfn "locked in thread 1"
//         lock monitor (fun () -> a:= !a + 2)
//         printfn "unlocked in thread 1"
//         )

//     let t2 = System.Threading.Thread (fun () ->
//         printfn "locked in thread 2"
//         lock monitor (fun () -> a:= !a - 3)
//         printfn "unlocked in thread 2"
//         )

//     t1.Start()
//     t2.Start()

//     System.Threading.Thread.Sleep 1000 // wait long enough to get the correct value
//     printfn "2) a = %i" !a

//     true

/// CookieContainer небезопасно использовать в параллельных вычислениях. Что делать? Включил, но использовать на свой страх и риск.
let getAsync reqf (urls: Url list) =
    let get url =
        let req : HttpWebRequest = reqf (createHttpReq url)

        req.Headers.Add (System.Net.HttpRequestHeader.AcceptEncoding, "gzip")
        async {
            try
                let! resp = req.AsyncGetResponse()
                use resp = resp :?> HttpWebResponse
                let encoding = defineEncoding resp.ContentType
                match resp.Headers.["Content-Encoding"] with
                | "gzip" ->
                    try
                        use receiveStream = resp.GetResponseStream()
                        use zl = new System.IO.Compression.GZipStream(receiveStream,System.IO.Compression.CompressionMode.Decompress)
                        use r = new System.IO.StreamReader(zl, encoding)
                        let str = r.ReadToEnd()
                        return (url, Right(resp.StatusCode, str) : Res)
                    with e -> return url, Left e
                | _ -> // `null` возьмет?
                    use stream = resp.GetResponseStream()
                    use reader = new System.IO.StreamReader(stream, encoding)
                    return (url, Right(resp.StatusCode, reader.ReadToEnd()) : Res)
            with e -> return (url, Left e : Res)
        }
    let webPages = List.map get >> Async.Parallel >> Async.RunSynchronously
    List.chunkBySize 8 urls
    |> Seq.collect webPages

let tryPost reqf (postData:string) (url:string) =
    let request : HttpWebRequest = reqf (createHttpReq url)

    request.Method <- "POST"
    request.ContentType <- "application/x-www-form-urlencoded"

    let data = System.Text.Encoding.ASCII.GetBytes(postData)
    request.ContentLength <- int64 data.Length

    request.Headers.Add (System.Net.HttpRequestHeader.AcceptEncoding, "gzip")
    try
        using (request.GetRequestStream())
            (fun stream -> stream.Write(data, 0, data.Length))

        use resp = request.GetResponse() :?> HttpWebResponse
        let encoding = defineEncoding resp.ContentType

        match resp.Headers.["Content-Encoding"] with
        | "gzip" ->
            try
                use receiveStream = resp.GetResponseStream()
                use zl = new System.IO.Compression.GZipStream(receiveStream,System.IO.Compression.CompressionMode.Decompress)
                use r = new System.IO.StreamReader(zl, encoding)
                let str = r.ReadToEnd()
                url, Right(resp.StatusCode, str) : Res
            with e -> url, Left e
        | _ -> // `null` возьмет?
            use stream = resp.GetResponseStream()
            use reader = new System.IO.StreamReader(stream, encoding)
            url, Right(resp.StatusCode, reader.ReadToEnd()) : Res
    with e -> url, Left e

(*
/// test
let mainUrl = @"http://www.google.com"
let enterUrl () =
    let main = mainUrl |> downloadWebpage |> XmlAnalyzer.htmlToXml
    let node = main.SelectSingleNode "//*[@id='enter_link']"
    let href = node.SelectSingleNode "./@href"
    let url = mainUrl + href.Value
    url |> downloadWebpage |> ignore
enterUrl ()

let urls =
    File.ReadLines @"picpages.txt"
    |> Seq.truncate 10 |> List.ofSeq
let fileNames = urls |> List.map (fun x -> let uri = UrlNew x in uri.Segments.[uri.Segments.Length - 1]) |> List.map (fun x -> x + ".html")
let pages = urls |> getAsync
List.zip fileNames pages |> List.iter (File.WriteAllText)
*)

// open System.Web;

// let getToFile (url:string) path rawPath =
//     if not rawPath && path <> "" then
//         Directory.CreateDirectory(path) |> ignore
//     let webReq = System.Net.WebRequest.CreateHttp(url)

//     //webReq.CookieContainer = cookie;

//     use response = webReq.GetResponse()
//     use stream = response.GetResponseStream()

//     /// <summary>
//     /// Copies the contents of input to output. Doesn't close either stream.
//     /// </summary>
//     let streamToStream ((input:Stream), (output:Stream)) =
//         let buffer = Array.create (8 * 1024) (byte 0)
//         let rec f () =
//             let len = input.Read(buffer, 0, buffer.Length)
//             if len > 0 then
//                 output.Write(buffer, 0, len)
//                 f()
//         f()

//     let path =
//         if rawPath then path
//         else
//             let segments = response.ResponseUri.Segments;
//             let segmentLast = segments.[segments.Length - 1];
//             let segmentLast = HttpUtility.UrlDecode(segmentLast);
//             path + segmentLast

//     using (File.Create(path))
//         (fun x -> streamToStream(stream, x))


module DownThemAll =
    open FsharpMyExtension.XmlBuilder
    // let f () =
    //     System.IO.File.ReadAllText @"e:\downloads\Downloads.meta4"
    //     |> FsharpMyExtension.HtmlNode.HtmlNode.ofString
    //     |> fun x -> x.ChildNodes
    //     |> Node.ofHtmlNodes
    //     |> Seq.map (sprintf "%A")
    //     |> fun cont -> System.IO.File.WriteAllLines("output/outputTemp.txt", cont)
    /// DownThemAll! 3.0v
    /// (path * url) list
    let metalink (xs:(string * string) seq) =
        let now = System.DateTime.Now
        let nowStr =
            now
            |> FsharpMyExtension.DateTime.Unix.toMSec
            |> string
        // let now = System.DateTime.Now
        // now.ToString("R")

        let file i (name, url) =
            Node
              ("file",
               [("name", name)
                ("a0:num", sprintf "%d" i)
                ("a0:startdate", nowStr) // "1524916470084");
                ("a0:referrer", "")],
               [
                //Node ("description",[],[Text ""]);
                Node
                  ("url",
                   [("priority", "100");
                    ("a0:usable", url)],
                   [TextStr url])
                ]);
        Node
          ("metalink",
           [("xmlns", "urn:ietf:params:xml:ns:metalink"); ("version", "4.0");
            ("a0:version", "3.0.8");
            ("xmlns:a0", "http://www.downthemall.net/properties#")],
           [yield Node ("generator",[],[Text "DownThemAll!/3.0"]);
            yield Node ("published",[],[Text (now.ToString("R"))]) //"Sat, 28 Apr 2018 11:55:16 GMT"]);
            yield! Seq.mapi file xs
           ])
        |> Node.sprintNodeXml
    /// filepath -> (path * url) list
    let metalinkf (path:string) (xs:(string * string) list) =
        metalink xs |> fun cont -> System.IO.File.WriteAllText(path, cont)
