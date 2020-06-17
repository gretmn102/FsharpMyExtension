module FsharpMyExtension.WebDownloader
open System
open System.Net
open System.IO
open Microsoft.FSharp.Control.WebExtensions
open FsharpMyExtension.Either

let cookies = CookieContainer()
ServicePointManager.DefaultConnectionLimit <- Int32.MaxValue

// код ради Uri "http://x/y." ибо в оригинальном Uri преобразует его в "http://x/y"
do
    let getSyntax = typeof<UriParser>.GetMethod("GetSyntax", System.Reflection.BindingFlags.Static ||| System.Reflection.BindingFlags.NonPublic);
    let flagsField = typeof<UriParser>.GetField("m_Flags", System.Reflection.BindingFlags.Instance ||| System.Reflection.BindingFlags.NonPublic);
    if (isNull getSyntax && isNull flagsField) then
        [ "http"; "https" ]
        |> List.iter (fun scheme ->
            let parser = getSyntax.Invoke(null, [| scheme |]) :?> UriParser
            if (isNull parser) then
                let flagsValue = flagsField.GetValue(parser) :?> int
                // Clear the CanonicalizeAsFilePath attribute
                if ((flagsValue &&& 0x1000000) <> 0) then
                    flagsField.SetValue(parser, flagsValue &&& ~~~0x1000000);
        )

type Url = string
type Content = string
type Res = Url * Either<Exception, HttpStatusCode * Content>
module Decription =
    let gzip (encoding:System.Text.Encoding) (receiveStream:Stream) =
        try
            // use receiveStream = resp.GetResponseStream()
            use zl = new System.IO.Compression.GZipStream(receiveStream,System.IO.Compression.CompressionMode.Decompress)
            use r = new System.IO.StreamReader(zl, encoding)
            let str = r.ReadToEnd()
            Right str
        with e -> Left e
let tryGet (encoding:System.Text.Encoding) (url:string) =
    let req = System.Net.WebRequest.CreateHttp(url)
    req.CookieContainer <- cookies
    req.Headers.Add (System.Net.HttpRequestHeader.AcceptEncoding, "gzip")
    try
        use resp = req.GetResponse() :?> HttpWebResponse
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
            use reader = new StreamReader(stream, encoding)
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
let getAsync (encoding:System.Text.Encoding) (urls: Url list) =
    let get url =
        let req = System.Net.WebRequest.CreateHttp(url:string)
        req.UserAgent <- "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:34.0) Gecko/20100101 Firefox/34.0"
        req.Accept <- "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
        req.Headers.Add (System.Net.HttpRequestHeader.AcceptLanguage, "ru-RU,ru;q=0.8,en-US;q=0.5,en;q=0.3")
        req.CookieContainer <- cookies
        req.Headers.Add (System.Net.HttpRequestHeader.AcceptEncoding, "gzip")
        async {
            try
                let! resp = req.AsyncGetResponse()
                use resp = resp :?> HttpWebResponse
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
                    use reader = new StreamReader(stream, encoding)
                    return (url, Right(resp.StatusCode, reader.ReadToEnd()) : Res)
            with e -> return (url, Left e : Res)
        }
    let webPages = List.map get >> Async.Parallel >> Async.RunSynchronously
    List.chunkBySize 8 urls
    |> Seq.collect webPages

/// раньше стояла кодировка "windows-1251"
let tryPost (encoding:System.Text.Encoding) (url:string) (postData:string) =
    let request = System.Net.WebRequest.CreateHttp(url)

    request.UserAgent <- "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:34.0) Gecko/20100101 Firefox/34.0"
    request.CookieContainer <- cookies;
    request.Method <- "POST"
    request.ContentType <- "application/x-www-form-urlencoded"

    let data = Text.Encoding.ASCII.GetBytes(postData)
    request.ContentLength <- int64 data.Length

    try
        using (request.GetRequestStream())
            (fun stream -> stream.Write(data, 0, data.Length))
        use resp = request.GetResponse() :?> HttpWebResponse

        use stream = resp.GetResponseStream()
        use reader = new StreamReader(stream, encoding)

        url, Right(resp.StatusCode, reader.ReadToEnd()) : Res
    with e -> url, Left e
let tryPost2 (encoding:System.Text.Encoding) changeReq (url:string) (postData:string) =
    // let request = request() :?>
    let request = System.Net.WebRequest.CreateHttp(url)
    changeReq request
    // request.UserAgent <- "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:34.0) Gecko/20100101 Firefox/34.0"
    request.CookieContainer <- cookies;
    request.Method <- "POST"
    request.ContentType <- "application/x-www-form-urlencoded"

    let data = Text.Encoding.ASCII.GetBytes(postData)
    request.ContentLength <- int64 data.Length

    try
        using (request.GetRequestStream())
            (fun stream -> stream.Write(data, 0, data.Length))
        use resp = request.GetResponse() :?> HttpWebResponse

        use stream = resp.GetResponseStream()
        use reader = new StreamReader(stream, encoding)

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
